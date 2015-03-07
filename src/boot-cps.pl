# A very literal encoding of CPS into Perl lambdas. Not fast at all. This
# literal encoding means we don't need to do our own lexical scope analysis or
# write a GC.

{

package ni::lisp;

use constant DEBUG => 1;

sub compile_list;
sub array_literal;
sub hash_literal;
sub qstr_literal;
sub str_literal;
sub var_reference;
sub num_literal;
sub function_call;

our $gensym_id = 0;
sub gensym { ($_[0] // 'gensym') . ++$gensym_id }

sub perlize_name { $_[0] =~ s/([^A-Za-z_])/"_".ord($1)/egr }

deftypemethod 'compile',
  list   => sub { compile_list  @{$_[0]} },
  array  => sub { array_literal map $_->compile, @{$_[0]} },
  hash   => sub { hash_literal  map $_->compile, @{$_[0]} },
  qstr   => sub { qstr_literal  ${$_[0]} },
  str    => sub { str_literal   ${$_[0]} },
  symbol => sub { var_reference ${$_[0]} },
  number => sub { num_literal   ${$_[0]} };

# CPS transformation happens at the macroexpansion level, so by this point the
# whole program is represented in terms of CPS lambdas and associated special
# forms.
our %special_forms = (
  'fn*' => sub {
    my ($formals, $body) = @_;
    die "formals must be specified as an array or symbol (got $formals)"
      unless ref $formals eq 'ni::lisp::array'
          || ref $formals eq 'ni::lisp::symbol';

    my $formal_binding =
      ref $formals eq 'ni::lisp::array'
        ? "my (" . join(', ', map "\$" . perlize_name($$_), @$formals)
                 . ") = \@_"
        : "my \$" . perlize_name($$formals) . " = \\\@_";
    my $compiled_body  = $body->compile;
    my $result_gensym  = gensym 'result';

    DEBUG ? qq{ sub {
              $formal_binding;
              my \$$result_gensym = eval {
                $compiled_body;
              };
              die q{((fn* $formals $body) }.join(" ", \@_).qq{): \$@} if \$@;
              \$$result_gensym;
            } }
          : qq{ sub {
              $formal_binding;
              $compiled_body;
            } };
  },

  'nth*' => sub {
    my ($k, $n, @vs) = map $_->compile, @_;
    my $v_options    = join ', ', @vs;
    qq{ ($k)->(($v_options)[$n]) };
  },

  # We have no concurrency in the bootstrap layer, so just execute each
  # continuation in sequence and collect results. This implementation is
  # semantically correct up to GC properties.
  'co*' => sub {
    my ($k, @fs) = map $_->compile, @_;
    my $k_gs     = gensym 'k';
    my $i_gs     = gensym 'indexes';
    my $n        = @fs;
    my @ks       = map qq[ sub { \$$i_gs]."{$_}".qq[ = \$_[0];
                                 \$$k_gs->(map \$$i_gs]."{\$_}".qq[, 0..$#_)
                                   if scalar(keys \%$i_gs) == $n; }],
                       0..$#fs;

    my $calls    = join "\n", map qq{ ($fs[$_])->($ks[$_]); }, 0..$#fs;

    qq{
      my \$$k_gs = $k;
      my \%$i_gs;
      $calls;
    };
  },

  # For now, choose the first alternative every time. Others don't need to even
  # be compiled because they're all semantically equivalent.
  'amb*' => sub {
    my ($k, $f) = map $_->compile, @_;
    qq{ ($f)->($k); };
  },
);

sub compile_list {
  my ($h, @xs) = @_;
  ref $h eq 'ni::lisp::symbol' && exists $special_forms{$$h}
    ? $special_forms{$$h}->(@xs)
    : function_call($h, @xs);
}

sub array_literal { "[" . join(', ', @_) . "]" }
sub hash_literal  { "{" . join(', ', @_) . "}" }
sub qstr_literal  { "'$_[0]'" }
sub str_literal   { "\"$_[0]\"" }
sub var_reference { "\$" . perlize_name($_[0]) }
sub num_literal   { $_[0] }

sub function_call {
  my ($f, @xs) = map $_->compile, @_;
  $f . "->(" . join(", ", @xs) . ")";
}

# I don't want to write the following in CPS, so here's a Perl-hosted version.
# This method assumes that your form has already been macroexpanded, and is not
# idempotent at all (!!!).
sub cps_wrap {
  my $k_gensym = symbol gensym 'k';
  list symbol('fn*'), array($k_gensym), $_[0]->cps_convert($k_gensym);
}

our %cps_special_forms = (
  'fn*' => sub {
    my ($k_form, $formals, $body) = @_;
    my $k_gensym = symbol gensym 'k';

    # Two possibilities. If the function is variadic, then we need to generate
    # runtime code that splits the continuation off the end and saves it.
    if (ref $formals eq 'ni::lisp::array') {
      # Easy case: just add an extra formal for the continuation.
      list $k_form,
           list symbol('fn*'),
                array($k_gensym, @$formals),
                $body->cps_convert($k_gensym);
    } else {
      # Hard case: generate CPS-converted runtime code.
      my $formals_gensym = symbol gensym 'kf';
      list $k_form,
           list symbol('fn*'),
                $formals_gensym,
                list symbol('uncons'),
                     list(symbol('fn*'),
                          array($k_gensym, $formals),
                          $body->cps_convert($k_gensym)),
                     $formals_gensym;
    }
  },

  'nth*' => sub {
    my $k_form = shift @_;
    my @gensyms = map symbol(gensym 'x'), @_;
    list symbol('co*'),
         list(symbol('fn*'),
              array(@gensyms),
              list symbol('nth*'), $k_form, @gensyms),
         map(cps_wrap($_), @_);
  },

  'co*' => sub {
    my $k_form = shift @_;
    list symbol('co*'), $k_form, map cps_wrap($_), @_;
  },

  'amb*' => sub {
    my $k_form = shift @_;
    list symbol('amb*'), $k_form, map cps_wrap($_), @_;
  },
);

sub cps_convert_call {
  my ($k_form, $f, @xs) = @_;
  my $f_gensym = symbol gensym(ref $f eq 'ni::lisp::symbol' ? $$f : 'f');
  my @gensyms = map symbol(gensym 'x'), @xs;
  list symbol('co*'),
       list(symbol('fn*'),
            array($f_gensym, @gensyms),
            list $f_gensym, $k_form, @gensyms),
       map cps_wrap($_), $f, @xs;
}

sub cps_constant {
  my ($self, $k_form) = @_;
  list $k_form, $self;
}

deftypemethod 'cps_convert',
  list => sub {
    my ($self, $k_form) = @_;
    my ($h, @xs) = @$self;
    ref $h eq 'ni::lisp::symbol' && exists $cps_special_forms{$$h}
      ? $cps_special_forms{$$h}->($k_form, @xs)
      : cps_convert_call $k_form, $h, @xs;
  },
  array => sub {
    my ($self, $k_form) = @_;
    my @gensyms = map symbol(gensym 'x'), @$self;
    list symbol('co*'),
         list(symbol('fn*'),
              array(@gensyms),
              list $k_form, array(@gensyms)),
         map cps_wrap($_), @$self;
  },
  hash => sub {
    my ($self, $k_form) = @_;
    my @gensyms = map symbol(gensym 'x'), @$self;
    list symbol('co*'),
         list(symbol('fn*'),
              array(@gensyms),
              list $k_form, hash(@gensyms)),
         map cps_wrap($_), @$self;
  },
  qstr   => \&cps_constant,
  str    => \&cps_constant,
  symbol => \&cps_constant,
  number => \&cps_constant;

}

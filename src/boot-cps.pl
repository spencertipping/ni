# A very literal encoding of CPS into Perl lambdas. Not fast at all. This
# literal encoding means we don't need to do our own lexical scope analysis or
# write a GC.

{

package ni::lisp;

sub compile_list;
sub array_literal;
sub hash_literal;
sub qstr_literal;
sub str_literal;
sub var_reference;
sub num_literal;
sub function_call;

our $gensym_id = 0;
sub gensym { '__' . ($_[0] // 'gensym') . '_' . ++$gensym_id }

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
    die "formals must be specified as an array (got $formals)"
      unless ref $formals eq 'ni::lisp::array';

    my $perlized_formals = join ', ', map "\$" . ($$_ =~ y/-/_/r), @$formals;
    my $compiled_body    = $body->compile;

    qq{ sub {
      my ($perlized_formals) = \@_;
      $compiled_body;
    } };
  },

  'nth*' => sub {
    my $k         = pop(@_)->compile;
    my ($n, @vs)  = map $_->compile, @_;
    my $v_options = join ', ', @vs;
    qq{ ($k)->(($v_options)[$n]) };
  },

  # We have no concurrency in the bootstrap layer, so just execute each
  # continuation in sequence and collect results.
  'co*' => sub {
    my $k    = pop(@_)->compile;
    my $k_gs = gensym 'k';
    my $i_gs = gensym 'indexes';
    my $n    = @_;
    my @ks   = map qq{ sub { \$$i_gs\{$_\} = \$_[0];
                             \$$k_gs->(map \$$i_gs\{\$_\}, 0..$#_)
                               if scalar(keys \%$i_gs) == $n; }},
                   0..$#_;

    my $calls = join "\n", map qq{ ($ks[$_])->($_[$_]); }, 0..$#_;

    qq{
      my \$$k_gs = $k;
      my \%$i_gs;
      $calls;
    };
  },

  # For now, choose the first alternative every time. Others don't need to even
  # be compiled because they're all semantically equivalent.
  'amb*' => sub {
    my $k = pop(@_)->compile;
    my $f = $_[0]->compile;
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
sub var_reference { "\$" . ($_[0] =~ y/-/_/r) }
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
    my ($formals, $body, $k_form) = @_;
    my $k_gensym = symbol gensym 'k';
    list $k_form,
         list symbol('fn*'),
              array(@$formals, $k_gensym),
              $body->cps_convert($k_gensym);
  },

  'nth*' => sub {
    my $k_form = pop @_;
    my @gensyms = map symbol(gensym 'x'), @_;
    list symbol('co*'),
         map(cps_wrap($_), @_),
         list symbol('fn*'),
              array(@gensyms),
              list symbol('nth*'), @gensyms, $k_form;
  },

  'co*' => sub {
    my $k_form = pop @_;
    list symbol('co*'), map(cps_wrap($_), @_), $k_form;
  },

  'amb*' => sub {
    my $k_form = pop @_;
    list symbol('amb*'), map(cps_wrap($_), @_), $k_form;
  },
);

sub cps_convert_call {
  list symbol('co*'),
       map(cps_wrap($_), @xs),
       list symbol('fn*'),
            array

}

ni::lisp::deftypemethod 'cps_convert',
  list => sub {
    my ($self, $k_form) = @_;
    my ($h, @xs) = @$self;
    ref $h eq 'ni::lisp::symbol' && exists $cps_special_forms{$$h}
      ? $cps_special_forms{$$h}->(@xs, $k_form)
      : cps_convert_call $h, @xs, $k_form;
  },

}

# Code generator
# This exists because I want gensyms and external references to be easier to
# deal with. It also supports some nice stuff like insertion points and
# peephole optimizations.

BEGIN {

sub ni::gen::new;
sub gen       { local $_; ni::gen->new(@_) }
sub gen_empty { gen('empty', {}, '') }

sub gen_seq {
  # Generates an imperative sequence of statements.
  my ($name, @statements) = @_;
  my $code_template = join ";\n", map "%\@x$_", 0 .. $#statements;
  my %subst;
  $subst{"x$_"} = $statements[$_] for 0 .. $#statements;
  gen($name, \%subst, $code_template);
}

{

package ni::gen;

use overload qw# %  subst  * map  @{} inserted_code_keys  "" compile
                 eq compile_equals #;

our $gensym_id  = 0;
our $randomness = join '', map sprintf("%04x", rand(65536)), 0..3;
sub gensym { '$' . ($_[0] // '') . "_${randomness}_" . $gensym_id++ }

sub parse_code;
sub new {
  my ($class, $sig, $refs, $code) = @_;
  my ($fragments, $gensyms, $gensym_indexes, $insertions) = parse_code $code;

  # Substitutions can be specified as refs, in which case we pull them out and
  # do a rewrite automatically (this is more notationally expedient than having
  # to do a % operation right away).
  my %subst;
  for (keys %$refs) {
    if (exists $$insertions{$_}) {
      $subst{$_} = $$refs{$_};
      delete $$refs{$_};
    }
  }

  exists $$gensyms{$_} or die "undefined ref $_ in $code" for keys %$refs;
  exists $$refs{$_}    or die    "unused ref $_ in $code" for keys %$gensyms;

  # NB: always copy the fragments because parse_code returns cached results
  bless({ sig               => $sig,
          fragments         => [@$fragments],
          gensym_names      => $gensyms,
          gensym_indexes    => $gensym_indexes,
          insertion_indexes => $insertions,
          inserted_code     => {},
          refs              => $refs // {} },
        $class) % {%subst};
}

sub genify {
  return $_[0] if ref $_[0] && $_[0]->isa('ni::gen');
  return ni::gen('genified', {}, $_[0]);
}

sub compile_equals {
  my ($self, $x) = @_;
  $x = $x->compile if ref $x;
  $self->compile eq $x;
}

sub share_gensyms_with {
  # Any intersecting gensyms from $g will be renamed to align with $self.
  # This directionality matters so multiple calls against $self will form a set
  # of mutually gensym-shared fragments.
  my ($self, $g) = @_;
  for ($$g{gensym_names}) {
    if (exists $$self{gensym_names}{$_}) {
      $$g{gensym_names}{$_} = $$self{gensym_names}{$_};
      ${$$g{fragments}}[$$g{gensym_indexes}{$_}] =
        ${$$self{fragments}}[$$self{gensym_indexes}{$_}];
    }
  }
  $self;
}

sub inherit_gensyms_from {
  $_[1]->share_gensyms_with($_[0]);
  $_[0];
}

sub build_ref_hash {
  my ($self, $refs) = @_;
  $refs //= {};
  $$refs{$$self{gensym_names}{$_}} = $$self{refs}{$_} for keys %{$$self{refs}};
  $$self{inserted_code}{$_}->build_ref_hash($refs) for @$self;
  $refs;
}

sub inserted_code_keys {
  my ($self) = @_;
  [sort keys %{$$self{inserted_code}}];
}

sub subst {
  my ($self, $vars) = @_;
  for my $k (keys %$vars) {
    die "unknown subst var: $k (code is $self)"
      unless defined(my $i = $$self{insertion_indexes}{$k});
    $$self{inserted_code}{$k} = $$self{fragments}[$i] = genify $$vars{$k};
  }
  $self;
}

sub map {
  my ($self, $f) = @_;
  $f = ni::compile $f;
  my $y = &$f($self);
  return $y unless $y eq $self;

  # If we haven't changed, then operate independently on the
  # already-substituted code fragments and build a new instance.
  my $new = bless {}, ref $self;
  $$new{$_} = $$self{$_} for keys %$self;
  $$new{fragments}     = [@{$$new{fragments}}];
  $$new{inserted_code} = {%{$$new{inserted_code}}};

  $new % {map {$_, $$new{inserted_code}{$_} * $f} @$new};
}

sub compile {
  my ($self) = @_;
  join '', @{$$self{fragments}};
}

sub run {
  my ($self) = @_;
  my $code     = $self->compile;
  my $refs     = $self->build_ref_hash;
  my $bindings = join "\n", map sprintf("my %s = \$_[0]->{'%s'};", $_, $_),
                                keys %$refs;
  my $f        = eval($code = "package main; sub {\n$bindings\n$code\n}");
  die "$@ compiling\n$code" if $@;

  my @result = &$f($refs);
  delete $$refs{$_} for keys %$refs;    # we create circular refs sometimes
  @result;
}

our %parsed_code_cache;
sub parse_code {
  # Returns ([@code_fragments], {gensym_mapping}, {insertion_indexes})
  my ($code) = @_;
  return @$_ if defined($_ = $parsed_code_cache{$code});

  my @pieces = split /(\%:\w+|\%\@\w+)/, $code;
  my @fragments;
  my %gensyms;
  my %gensym_indexes;
  my %insertion_points;
  for (0..$#pieces) {
    if ($pieces[$_] =~ /^\%:(\w+)$/) {
      $gensym_indexes{$1} = $_;
      push @fragments, $gensyms{$1} = gensym $1;
    } elsif ($pieces[$_] =~ /^\%\@(\w+)$/) {
      $insertion_points{$1} = $_;
      push @fragments, "\ndie 'unfilled fragment: %\@$1';\n";
    } else {
      push @fragments, $pieces[$_];
    }
  }
  @{$parsed_code_cache{$code} = [[@fragments],
                                 {%gensyms},
                                 {%gensym_indexes},
                                 {%insertion_points}]};
}

}

}

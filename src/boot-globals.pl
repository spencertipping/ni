# Takes a regular Perl sub and makes it work with CPS
sub cps {
  my ($f) = @_;
  sub {
    my $k = pop @_;
    $k->($f->(@_));
  };
}

sub defcps { ${$_[0]} = cps $_[1] }

defcps 'gensym',  sub { ni::lisp::symbol ni::lisp::gensym @_ };
defcps 'sym_str', sub { ni::lisp::str    ${$_[0]} };
defcps 'str_sym', sub { ni::lisp::symbol ${$_[0]} };

defcps 'car',   sub { my ($l) = @_; $$l[0] };
defcps 'cdr',   sub { my ($l) = @_; ni::lisp::list(@$l[1..$#{$l}]) };
defcps 'cons',  sub { my ($a, $d) = @_; ni::lisp::list($a, @$d) };
defcps 'list',  sub { ni::lisp::list(@_) };
defcps 'nil',   sub { ni::lisp::list() };
defcps 'consp', sub { @{$_[0]} > 0 };
defcps 'count', sub { scalar(@{$_[0]}) };
defcps 'eq',    sub { "$_[0]" eq "$_[1]" ? 1 : 0 };
defcps 'not',   sub { $_[0] ? 0 : 1 };
defcps 'print', sub { print join(' ', @_), "\n" };

$ni::lisp::macros{def_} = sub {
  my ($name, $value, $k) = @_;
  my $m = $value->macroexpand;
  my $c = $m->compile;
  ${$$name =~ y/-/_/r} = eval $c;
  die "failed to compile $name -> $m -> $c: $@" if $@;
  $k->($name);
};

$ni::lisp::macros{defmacro_} = sub {
  my ($name, @stuff) = @_;
  $ni::lisp::macros{def_}->(@_);
  $ni::lisp::macros{$$name} = ${$$name};
};

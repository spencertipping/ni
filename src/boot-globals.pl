# Takes a regular Perl sub and makes it work with CPS
sub cps {
  my ($f) = @_;
  sub {
    my $k = pop @_;
    $k->($f->(@_));
  };
}

sub defcps { (eval "sub {\$$_[0] = \$_[0]}")->(cps $_[1]) }

defcps 'gensym',  sub { ni::lisp::symbol ni::lisp::gensym @_ };
defcps 'sym_str', sub { ni::lisp::str ${$_[0]} };
defcps 'str_sym', sub { ni::lisp::symbol $_[0] };

defcps 'to_array', sub { ni::lisp::array @{$_[0]} };
defcps 'to_hash',  sub { ni::lisp::hash  @{$_[0]} };
defcps 'to_list',  sub { ni::lisp::list  @{$_[0]} };

defcps 'type', sub { ref($_[0]) =~ s/.*:://r };

defcps 'car',   sub { my ($l) = @_; $$l[0] };
defcps 'cdr',   sub { my ($l) = @_; ni::lisp::list(@$l[1..$#{$l}]) };
defcps 'cons',  sub { my ($a, $d) = @_; ni::lisp::list($a, @$d) };
defcps 'list',  sub { ni::lisp::list(@_) };
defcps 'nil',   sub { ni::lisp::list() };
defcps 'count', sub { scalar(@{$_[0]}) };
defcps 'eq',    sub { "$_[0]" eq "$_[1]" ? 1 : 0 };
defcps 'gt',    sub { $_[0] > $_[1] ? 1 : 0 };
defcps 'not',   sub { $_[0] ? 0 : 1 };
defcps 'print', sub { print join(' ', @_), "\n" };

defcps 'macroexpand', sub { $_[0]->macroexpand };
defcps 'eval',        sub { my $c = $_[0]->compile;
                            my $r = eval $c;
                            die "failed to eval $_[0] -> $c: $@" if $@;
                            $r };

defcps 'defcps_', sub {
  my ($name, $value) = @_;
  ${$name =~ y/-/_/r} = $value;
  $name;
};

defcps 'defmacrocps_', sub {
  my ($name, $value) = @_;
  $ni::lisp::macros{$name} = $value;
  $name;
};

defcps 'cps_convert', sub { $_[0]->cps_convert($_[1]) };

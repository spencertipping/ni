# Takes a regular Perl sub and makes it work with CPS
sub cps {
  my ($f) = @_;
  sub {
    my $k = pop @_;
    $k->($f->(@_));
  };
}

sub defcps { ${ni::lisp::perlize_name $_[0]} = cps $_[1] }

defcps 'gensym',  sub { ni::lisp::symbol ni::lisp::gensym @_ };
defcps 'sym-str', sub { ni::lisp::str ${$_[0]} };
defcps 'str-sym', sub { ni::lisp::symbol $_[0] };

defcps 'to-array', sub { ni::lisp::array @{$_[0]} };
defcps 'to-hash',  sub { ni::lisp::hash  @{$_[0]} };
defcps 'to-list',  sub { ni::lisp::list  @{$_[0]} };

defcps 'type', sub { ref($_[0]) =~ s/.*:://r };

defcps 'car',   sub { my ($l) = @_; $$l[0] };
defcps 'cdr',   sub { my ($l) = @_; ni::lisp::list(@$l[1..$#{$l}]) };
defcps 'cons',  sub { my ($a, $d) = @_; ni::lisp::list($a, @$d) };
defcps 'list',  sub { ni::lisp::list(@_) };
defcps 'nil',   sub { ni::lisp::list() };
defcps 'count', sub { scalar(@{$_[0]}) };
defcps '=',     sub { "$_[0]" eq "$_[1]" ? 1 : 0 };
defcps '>',     sub { $_[0] > $_[1] ? 1 : 0 };
defcps 'not',   sub { $_[0] ? 0 : 1 };
defcps 'print', sub { print join(' ', @_), "\n" };

defcps 'macroexpand', sub { $_[0]->macroexpand };
defcps 'eval',        sub { my $c = $_[0]->compile;
                            my $r = eval $c;
                            die "failed to eval $_[0] -> $c: $@" if $@;
                            $r };

defcps 'defcps*', sub {
  my ($name, $value) = @_;
  ${ni::lisp::perlize_name $name} = $value;
  $name;
};

defcps 'defmacrocps*', sub {
  my ($name, $value) = @_;
  $ni::lisp::macros{$name} = $value;
  $name;
};

defcps 'cps-convert', sub { $_[0]->cps_convert($_[1]) };

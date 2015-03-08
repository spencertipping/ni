# Takes a regular Perl sub and makes it work with CPS
sub cps {
  my ($f) = @_;
  sub { $_[0]->($f->(@_[1..$#_])) };
}

sub def    { ${ni::lisp::perlize_name $_[0]} = $_[1] }
sub defcps { def $_[0], cps $_[1] }

defcps 'gensym',  sub { ni::lisp::symbol ni::lisp::gensym(@_ ? ${$_[0]} : undef) };
defcps 'sym-str', sub { ni::lisp::str    ${$_[0]} };
defcps 'str-sym', sub { ni::lisp::symbol ${$_[0]} };

defcps 'to-array', sub { ni::lisp::array @{$_[0]} };
defcps 'to-hash',  sub { ni::lisp::hash  @{$_[0]} };
defcps 'to-list',  sub { ni::lisp::list  @{$_[0]} };

defcps 'aget',   sub { $_[0]->[${$_[1]}] };
defcps 'type',   sub { ni::lisp::str(ref($_[0]) =~ s/.*:://r) };

defcps 'car',    sub { my ($l) = @_; $$l[0] };
defcps 'cdr',    sub { my ($l) = @_; ni::lisp::list(@$l[1..$#{$l}]) };
defcps 'cons',   sub { my ($a, $d) = @_; ni::lisp::list($a, @$d) };
defcps 'uncons', sub { my ($l) = @_; ($$l[0], ni::lisp::list(@$l[1..$#{$l}])) };
defcps 'list',   sub { ni::lisp::list(@_) };
defcps 'count',  sub { ni::lisp::number scalar(@{$_[0]}) };
defcps '=',      sub { ni::lisp::number("$_[0]" eq "$_[1]" ? 1 : 0) };
defcps '>',      sub { ni::lisp::number(${$_[0]} > ${$_[1]} ? 1 : 0) };
defcps 'not',    sub { ni::lisp::number($_[0]->truthy ? 0 : 1) };
defcps 'print',  sub { my $x = print STDERR join(' ', @_), "\n";
                       ni::lisp::number $x };

defcps $_, eval "sub { ni::lisp::number(\${\$_[0]} $_ \${\$_[1]}) }"
  for qw# + - * / % ** << >> ^ & | #;

defcps 'macroexpand', sub { $_[0]->macroexpand };
defcps 'eval',        sub { my $c = ni::lisp::compile $_[0];
                            my $r = eval $c;
                            die "failed to eval $_[0] -> $c: $@" if $@;
                            $r };

def 'apply', sub {
  my ($k, $f, @xs) = @_;
  my @ys = @{pop @xs};
  $f->($k, @xs, @ys);
};

defcps 'macro-fn', sub { $ni::lisp::macros{${$_[0]}} };

defcps 'defcps*', sub {
  my ($name, $value) = @_;
  ${ni::lisp::perlize_name $$name} = $value;
  $name;
};

defcps 'defmacrocps*', sub {
  my ($name, $value) = @_;
  $ni::lisp::macros{$$name} = $value;
  $name;
};

defcps 'cps-convert', sub { $_[0]->cps_convert($_[1]) };

def 'nil', ni::lisp::list();

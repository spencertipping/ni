# Bootstrap concatenative compiler
package nb;

{ package nb::val; use overload qw/ "" str / }

push @nb::string::ISA, 'nb::val', 'nb::one', 'nb::stable';
push @nb::number::ISA, 'nb::val', 'nb::one', 'nb::stable';
push @nb::symbol::ISA, 'nb::val', 'nb::one';
push @nb::list::ISA,   'nb::val', 'nb::many', 'nb::stable';
push @nb::array::ISA,  'nb::val', 'nb::many', 'nb::stable';
push @nb::hash::ISA,   'nb::val', 'nb::many', 'nb::stable';

sub nb::list::delimiters  { '(', ')' }
sub nb::array::delimiters { '[', ']' }
sub nb::hash::delimiters  { '{', '}' }

sub nb::string::str { "\"${$_[0]}\"" }
sub nb::one::str    { ${$_[0]} }
sub nb::many::str {
  my ($self) = @_;
  my ($open, $close) = $self->delimiters;
  $open . join(' ', @$self) . $close;
}

sub unquote;
sub nb::stable::eval { $_[1]->($_[0], @_[2..$#_]) }
sub nb::symbol::eval {
  my ($self, $k, @stuff) = @_;
  my $v = ${$$self} // die "can't eval undefined symbol $self";
  unquote $k, $v, @stuff;
}

sub unquote {
  my ($k, $v, @stuff) = @_;
  return $v->($k, @stuff) if ref($v) eq 'CODE';
  $eval->($k, $v, @stuff);
}

sub string { my ($x) = @_; bless \$x, 'nb::string' }
sub number { my ($x) = @_; bless \$x, 'nb::number' }
sub symbol { my ($x) = @_; bless \$x, 'nb::symbol' }
sub list   { bless [@_], 'nb::list'  }
sub array  { bless [@_], 'nb::array' }
sub hash   { bless [@_], 'nb::hash'  }

our %bracket_types = ( ')' => \&list,
                       ']' => \&array,
                       '}' => \&hash );

sub parse {
  local $_;
  my @stack = [];
  while ($_[0] =~ /\G (?: (?<comment> \#[!\s].*)
                        | (?<ws>      [:,\s]+)
                        | (?<string>  "(?:[^\\"]|\\.)*")
                        | (?<number>  [-+]?[0-9]\S*)
                        | (?<symbol>  [^"()\[\]{}\s,:]+)
                        | (?<opener>  [(\[{])
                        | (?<closer>  [)\]}]))/gx) {
    my $k = (keys %+)[0];
    next if $k eq 'comment' || $k eq 'ws';
    if ($k eq 'opener') {
      push @stack, [];
    } elsif ($k eq 'closer') {
      my $last = pop @stack;
      die "too many closing brackets" unless @stack;
      push @{$stack[-1]}, $bracket_types{$+{closer}}->(@$last);
    } else {
      push @{$stack[-1]}, &{"nb::$k"}($+{$k});
    }
  }
  die "unbalanced brackets: " . scalar(@stack) . " != 1"
    unless @stack == 1;
  @{$stack[0]};
}

# Interpreter and REPL (CPS)
END {
  select((select(STDERR), $|++)[0]);
}

# Global functions
# All defined in CPS, which is also how the REPL works.
sub def {
  my ($name, $f) = @_;
  ${$name} = sub {
    my ($k, @xs) = @_;
    unquote sub {$_[0]}, $k, $f->(@xs);
  };
}

# Stack manipulation
def 'swap', sub { @_[1, 0, 2..$#_] };
def 'dup',  sub { @_[0, 0, 1..$#_] };
def 'drop', sub { @_[1..$#_] };

# Eval, definition, and conditionals
$eval = sub {
  my ($k, $x, @stuff) = @_;
  die "argument to eval must be an array (got $x)"
    unless ref($x) eq 'nb::array';

  if (@$x) {
    # [f g h] is evaluated as f([g h], @stack), which uses the array [g h] as a
    # continuation.
    my ($f, @xs) = @$x;
    $f->eval($k, ni::array(@xs), @stuff);
  } else {
    # ?????
    $k->(@stuff);
  }
};

def 'def', sub {
  my ($name, $v, @stuff) = @_;
  ${$name} = $v;
  @stuff;
};

# Used for conditionals in conjunction with 'eval'
def 'nth', sub {
  my ($n, @stuff) = @_;
  $stuff[$$n], @stuff;
};

# Scalar value stuff
def 'type', sub { ref($_[0]) =~ s/^.*:://r };

def 'ys', sub { string(${$_[0]}), @_[1..$#_] };
def 'sy', sub { symbol(${$_[0]}), @_[1..$#_] };
def 'ns', sub { string(${$_[0]}), @_[1..$#_] };
def 'sn', sub { number(${$_[0]}), @_[1..$#_] };

def 'sv', sub { array(parse ${$_[0]}), @_[1..$#_] };
def 'vs', sub { string($_[0]->str), @_[1..$#_] };

def 'print', sub {
  my ($x, @stuff) = @_;
  print $$x, "\n";
  @stuff;
};

def 'prstack', sub {
  printf STDERR "%02d: %s\n", $_, $_[$_] for 0..$#_;
  @_;
};

# Multi-value stuff
def 'count', sub {
  my ($xs, @stuff) = @_;
  number(scalar @$xs), @stuff;
};

def 'xl', sub { list(@{$_[0]}),  @_[1..$#_] };
def 'xa', sub { array(@{$_[0]}), @_[1..$#_] };
def 'xh', sub { hash(@{$_[0]}),  @_[1..$#_] };

def 'lcons', sub {
  my ($x, $xs, @stuff) = @_;
  list($x, @$xs), @stuff;
};

def 'acons', sub {
  my ($x, $xs, @stuff) = @_;
  array($x, @$xs), @stuff;
};

def 'uncons', sub {
  my ($xs, @stuff) = @_;
  my ($h, @t) = @$xs;
  $h, &{ref $xs}(@t), @stuff;
};

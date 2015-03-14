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

sub nb::many::seq { @{$_[0]} }
sub nb::list::seq {
  my @result;
  for (my ($self) = @_; @$self; $self = $$self[1]) {
    push @result, $$self[0];
  }
  @result;
}

sub string { my ($x) = @_; bless \$x, 'nb::string' }
sub number { my ($x) = @_; bless \$x, 'nb::number' }
sub symbol { my ($x) = @_; bless \$x, 'nb::symbol' }
sub list   { @_ ? bless [$_[0], list(@_[1..$#_])], 'nb::list'
                : bless [], 'nb::list' }
sub array  { bless [@_], 'nb::array' }
sub hash   { bless [@_], 'nb::hash'  }

our $nil = list;

our %bracket_types = ( ')' => \&list,
                       ']' => \&array,
                       '}' => \&hash );

sub parse {
  local $_;
  my @stack = [];
  while ($_[0] =~ /\G (?: (?<comment> \#[!\s].*)
                        | (?<ws>      [,\s]+)
                        | (?<string>  "(?:[^\\"]|\\.)*":?)
                        | (?<number>  [-+]?[0-9]\S*)
                        | (?<symbol>  [^"()\[\]{}\s,]+)
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

# Interpreter and continuations
# All of the relatively straightforward code above has probably suggested that
# this interpreter consists of a simple loop that runs through lists, executing
# each thing and moving on to the next. And indeed that's what I at first
# thought it would be, but that's hardly a dangerous way to live. One might say
# it's not really living at all.
#
# No no, this interpreter is treacherous and subtle, the way an interpreter
# should be. We have a few degrees of freedom at this point, so I'll constrain
# the space a little:
#
# 1. We have reusable continuations that can be inspected and constructed (more
#    powerful than CPS; this gives us runtime macros that can transform their
#    dynamic scope).
# 2. All values and symbol bindings are immutable; to get (something that
#    appears to be) mutability, you need to construct an alternative
#    continuation and invoke it.
# 3. The symbol table is a function that takes a symbol and returns its
#    resolution. There is no default mechanism to handle unresolvable symbols,
#    and the symbol table as an object is one of the elements of a
#    continuation.

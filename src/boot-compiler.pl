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

sub string { my ($x) = @_; bless \$x, 'nb::string' }
sub number { my ($x) = @_; bless \$x, 'nb::number' }
sub symbol { my ($x) = @_; bless \$x, 'nb::symbol' }
sub list   { bless [@_], 'nb::list'  }
sub array  { bless [@_], 'nb::array' }
sub hash   { bless [@_], 'nb::hash'  }

our $nil = array();

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

# Interpreter/eval logic
# Writing a Joy interpreter without continuations is fairly straightforward if
# a little subtle. Continuations complicate matters a little, however,
# particularly given that they need to be really fast and that they are the
# mechanism by which we implement applicative-notation macros.
#
# In a concatenative language, a continuation is a pair of the current data
# stack and the current return stack. In our case, everything is executed
# inside a list, so each element in the return stack will be the tail of some
# list we've started to evaluate. For example:
#
# [foo bar [woot1 woot2] call/cc bif] eval baz bok
#
# In this program, [woot1 woot2] is called on the following:
#
# [[bif] [baz bok]]     # return stack as a list
# [...]                 # data stack as a list
# ...                   # everything else on the stack
#
# What's interesting about this is that [woot1 woot2] is at liberty to look at
# its continuation, create a new one, and call into that -- in effect
# interpreting future code. This produces strange but useful possibilities like
# this:
#
# [fn [x] (inc x)]
#
# where 'fn' takes its quoted continuation, [[x] (inc x)], reinterprets it into
# concatenative notation, and calls into that continuation instead.
#
# Ok, so where does all of this leave us? Well, we need the interpreter to be
# able to produce and consume continuations efficiently. In the applicative
# world this is done using CPS, an abstraction whose concatenative analogue is
# a little elusive (in my opinion anyway). The quoted-concatenative paradigm
# defies this kind of all-at-once transformation in any case, since we won't
# know when a list is intended as code or as data.
#
# This means that to the extent that we're CPS-transforming at all, we need to
# do it at the interpretation level.

# Bootstrap concatenative interpreter
#
# Semantics:
# Numbers and strings are self-quoting; symbols prefixed with ' are
# self-quoting, otherwise they resolve+evaluate when executed. Lists, arrays,
# and hashes are all self-quoting. All values appear to be immutable and all
# functions appear to be pure.
#
# Continuations are available and encoded using values that can be converted to
# and from 3-arrays. As in Scheme, invoking a continuation causes it to replace
# the current default one. Also as in Scheme, tail recursion is required;
# although continuations have structured views of the return stack, the return
# stack will never contain an empty list.

package nb;

{ package nb::val; use overload qw/ "" str / }

push @nb::string::ISA, qw/ nb::val nb::one  nb::stable /;
push @nb::number::ISA, qw/ nb::val nb::one  nb::stable /;
push @nb::symbol::ISA, qw/ nb::val nb::one             /;
push @nb::list::ISA,   qw/ nb::val nb::many nb::stable /;
push @nb::array::ISA,  qw/ nb::val nb::many nb::stable /;
push @nb::hash::ISA,   qw/ nb::val nb::many nb::stable /;

sub nb::list::delimiters  { '(', ')' }
sub nb::array::delimiters { '[', ']' }
sub nb::hash::delimiters  { '{', '}' }

sub nb::string::str { "\"${$_[0]}\"" }
sub nb::one::str    { ${$_[0]} }
sub nb::many::str {
  my ($self) = @_;
  my ($open, $close) = $self->delimiters;
  $open . join(' ', $self->seq) . $close;
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
sub number { my ($x) = @_;
             $x = hex $x if $x =~ /^0x[0-9a-fA-F]+$/;
             bless \$x, 'nb::number' }
sub symbol { my ($x) = @_; bless \$x, 'nb::symbol' }
sub list   { @_ ? bless [$_[0], list(@_[1..$#_])], 'nb::list'
                : bless [], 'nb::list' }
sub array  { bless [@_], 'nb::array' }
sub hash   { bless [@_], 'nb::hash'  }

our %bracket_types = ( ')' => \&list,
                       ']' => \&array,
                       '}' => \&hash );

sub parse {
  local $_;
  my @stack = [];
  my @tags;
  while ($_[0] =~ /\G (?: (?<comment> \#[!\s].*)
                        | (?<ws>      [,\s]+)
                        | (?<string>  "(?:[^\\"]|\\.)*":?)
                        | (?<number>  [-+]?[0-9][-+0-9a-zA-Z]*)
                        | (?<symbol>  '*[^"()\[\]{}\s,]+)
                        | (?<opener>  [^"()\[\]{}\s,]*[(\[{])
                        | (?<closer>  [)\]}]))/gx) {
    my $k = (keys %+)[0];
    next if $k eq 'comment' || $k eq 'ws';
    if ($k eq 'opener') {
      push @stack, [];
      push @tags, $+{opener} =~ s/.$//r;
    } elsif ($k eq 'closer') {
      my $last = pop @stack;
      die "too many closing brackets" unless @stack;
      push @{$stack[-1]}, $bracket_types{$+{closer}}->(pop(@tags), @$last);
    } else {
      push @{$stack[-1]}, &{"nb::$k"}($+{$k});
    }
  }
  die "unbalanced brackets: " . scalar(@stack) . " != 1" unless @stack == 1;
  list @{$stack[0]};
}

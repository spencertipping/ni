# ni lisp reader
# Produces a tree of blessed references representing the specified
# s-expression. Syntactically:
#
#   "foo\tbar"  string with tab character
#   foo         quoted atom (analogous to 'foo in lisp)
#   $foo        variable reference (analogous to foo in lisp)
#   3.0         numeric atom
#   [3 4 5]     array
#   {foo bar}   hash

{

package ni::lisp;

sub only_refs {
  my ($parens, @xs) = @_;
  my ($o, $c) = split //, $parens;
  defined $_ or die "found an undef in $o@xs$c" for @xs;
  ref($_) =~ /^ni::/ or ref($_) eq 'CODE'
    or die "found an invalid ref $_ (" . ref($_) . ") in $o@xs$c"
    for @xs;
  @xs;
}

# NB: these are not perl OO constructors in the usual sense (i.e. they can't be
# called indirectly)
sub list   { bless [only_refs '()', @_], "ni::lisp::list" }
sub array  { bless [only_refs '[]', @_], "ni::lisp::array" }
sub hash   { bless {only_refs '{}', @_}, "ni::lisp::hash" }

sub str    { my ($x) = @_; bless \$x, "ni::lisp::str" }
sub symbol { my ($x) = @_; bless \$x, "ni::lisp::symbol" }
sub number { my ($x) = @_; bless \$x, "ni::lisp::number" }

our @parse_types = qw/ list array hash str symbol number /;
our %overloads   = qw/ "" str @{} sequential /;

for (@parse_types) {
  eval "package ni::lisp::$_; use overload qw#" . join(' ', %overloads) . "#;";
  die $@ if $@;
}

push @{"ni::lisp::${_}::ISA"}, "ni::lisp::val" for @parse_types;

sub deftypemethod {
  my ($name, %alternatives) = @_;
  *{"ni::lisp::${_}::$name"} = $alternatives{$_} // sub { 0 } for @parse_types;
}

sub to_str { defined $_[0] ? ref($_[0]) =~ /^ni::/ ? $_[0]->str
                                                   : "<unblessed: $_[0]>"
                           : '<undef>' }
deftypemethod 'str',
  list   => sub { '(' . join(' ', map to_str($_), @{$_[0]}) . ')' },
  array  => sub { '[' . join(' ', map to_str($_), @{$_[0]}) . ']' },
  hash   => sub { '{' . join(' ', map {$_ => to_str(${$_[0]}{$_})}
                                      sort keys %{$_[0]}) . '}' },
  str    => sub { '"' . ${$_[0]} . '"' },
  symbol => sub { length ${$_[0]} ? ${$_[0]} : '<ESYM>' },
  number => sub { ${$_[0]} };

sub indent { '  ' x $_[0] }
sub pprint { defined $_[0] ? $_[0]->pprint($_[1]) : '<undef>' }

deftypemethod 'pprint',
  list   => sub { indent($_[1]) .
                  (length($_[0]->str) > 80 - 2 * $_[1]
                    ? "(\n"
                      . join("\n", map pprint($_, $_[1] + 1), @{$_[0]})
                      . ")"
                    : $_[0]->str) },

  array  => sub { indent($_[1]) .
                  (length($_[0]->str) > 80 - 2 * $_[1]
                    ? "[\n"
                      . join("\n", map pprint($_, $_[1] + 1), @{$_[0]})
                      . "]"
                    : $_[0]->str) },

  hash   => sub { indent($_[1]) .
                  (length($_[0]->str) > 80 - 2 * $_[1]
                    ? "{\n"
                      . join("\n", map pprint($_, $_[1] + 1),
                                   map "$_ ${$_[0]}{$_}", sort keys %{$_[0]})
                      . "}"
                    : $_[0]->str) },

  str    => sub { indent($_[1]) . $_[0] },
  symbol => sub { indent($_[1]) . $_[0] },
  number => sub { indent($_[1]) . $_[0] };

deftypemethod 'sequential',
  list  => sub { $_[0] },
  array => sub { $_[0] },
  hash  => sub { [map {(ni::lisp::parse $_)[0], ${$_[0]}{$_}}
                      sort keys %{$_[0]}] };

our %bracket_types = (
  ')' => \&ni::lisp::list,
  ']' => \&ni::lisp::array,
  '}' => \&ni::lisp::hash,
);

sub parse {
  local $_;
  my @stack = [];
  while ($_[0] =~ / \G (?: (?<comment> \#.*)
                         | (?<ws>      [\s,:]+)
                         | "(?<str>    (?:[^\\"]|\\.)*)"
                         | (?<number>  (?: [-+]?[0-9]*\.[0-9]+([eE][0-9]+)?
                                         | 0x[0-9a-fA-F]+
                                         | 0[0-7]+
                                         | [1-9][0-9]*
                                         | 0))
                         | (?<symbol>  [^"()\[\]{}\s,:]+)
                         | (?<opener>  [(\[{])
                         | (?<closer>  [)\]}])) /gx) {
    next if exists $+{comment} || exists $+{ws};
    if ($+{opener}) {
      push @stack, [];
    } elsif ($+{closer}) {
      my $last = pop @stack;
      die "too many closing brackets" unless @stack;
      push @{$stack[-1]}, $bracket_types{$+{closer}}->(@$last);
    } else {
      my @types = keys %+;
      my $v     = $+{$types[0]};
      die "FIXME: got @types" unless @types == 1;
      push @{$stack[-1]}, &{"ni::lisp::$types[0]"}($v);
    }
  }
  die "unbalanced brackets: " . scalar(@stack) . " != 1"
    unless @stack == 1;
  @{$stack[0]};
}

}

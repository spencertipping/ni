# ni lisp reader
# Produces a tree of blessed references representing the specified
# s-expression. Syntactically:
#
#   'foo\nbar'          string with a literal backslash-n in it
#   "foo\tbar"          string with tab character
#   foo                 quoted atom (analogous to 'foo in lisp)
#   $foo                variable reference (analogous to foo in lisp)
#   3.0                 numeric atom
#   [3 4 5]             array
#   {foo bar}           hash
#
# As in Clojure, everything is immutable. The JIT's job is to figure out how to
# make this acceptably fast, possibly by compiling to something besides Perl.

{

package ni::lisp;

# NB: these are not perl OO constructors in the usual sense (i.e. they can't be
# called indirectly)
sub list   { bless \@_, "ni::lisp::list" }
sub array  { bless \@_, "ni::lisp::array" }
sub hash   { bless \@_, "ni::lisp::hash" }

sub qstr   { bless \$_[0], "ni::lisp::qstr" }
sub str    { bless \$_[0], "ni::lisp::str" }
sub symbol { bless \$_[0], "ni::lisp::symbol" }
sub number { bless \$_[0], "ni::lisp::number" }

our @parse_types = qw/ list array hash qstr str symbol number /;
our %overloads   = qw/ "" str /;

for (@parse_types) {
  eval "package ni::lisp::$_; use overload qw#" . join(' ', %overloads) . "#;";
  DEBUG
  die $@ if $@;
  DEBUG_END
}

push @{"ni::lisp::${_}::ISA"}, "ni::lisp::val" for @parse_types;

sub deftypemethod {
  my ($name, %alternatives) = @_;
  *{"ni::lisp::${_}::$name"} = $alternatives{$_} // sub { 0 } for @parse_types;
}

deftypemethod 'str',
  list   => sub { '(' . join(' ', @{$_[0]}) . ')' },
  array  => sub { '[' . join(' ', @{$_[0]}) . ']' },
  hash   => sub { '{' . join(' ', @{$_[0]}) . '}' },
  qstr   => sub { "'" . ${$_[0]} . "'" },
  str    => sub { '"' . ${$_[0]} . '"' },
  symbol => sub { ${$_[0]} },
  number => sub { ${$_[0]} };

our %bracket_types = (
  ')' => \&ni::lisp::list,
  ']' => \&ni::lisp::array,
  '}' => \&ni::lisp::hash,
);

sub parse {
  local $_;
  my @stack = [];
  while ($_[0] =~ / \G (?: (?<comment> \#.*)
                         | (?<ws>      [\s,]+)
                         | '(?<qstr>   (?:[^\\']|\\.)*)'
                         | "(?<str>    (?:[^\\"]|\\.)*)"
                         | (?<number>  (?: [-+]?[0-9]*\.[0-9]+([eE][0-9]+)?
                                         | 0x[0-9a-fA-F]+
                                         | 0[0-7]+
                                         | [1-9][0-9]*))
                         | (?<symbol>  [^"()\[\]{}\s,]+)
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
      DEBUG
      die "FIXME: got @types" unless @types == 1;
      DEBUG_END
      push @{$stack[-1]}, &{"ni::lisp::$types[0]"}($v);
    }
  }
  die "unbalanced brackets: " . scalar(@stack) . " != 1"
    unless @stack == 1;
  @{$stack[0]};
}

}

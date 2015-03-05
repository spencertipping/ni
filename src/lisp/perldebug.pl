DEBUG

# Perl compiler backend (debug version, not for production at all)
# The compilation strategy works like this. Lambdas are explicitly represented
# using [$fn_index, $parent_ref, @args]; all function bodies are precompiled
# and none are Perl closures.
#
# Any closure references are created by linking to the current $parent_ref and
# passing in the current argument list.

{

package ni::lisp::perldebug;

BEGIN { *gen    = \&ni::lisp::gen;
        *gensym = \&ni::lisp::gen::gensym }

sub gen_array {
  gen('array', {map(("x$_" => $_[$_]), 0..$#_)},
    join(', ', map "%\@x$_", 0..$#_));
}

sub lexical_scope_deref {
  my ($args, $n) = @_;

}

sub new {
  my ($class) = @_;
  bless {fns     => [],
         globals => {}}, $class;
}

our %compilers = (
  fn => sub {
    # Create a global function based on the specified body and generate a
    # closure reference for it.
    my ($self, $node) = @_;
    my $i = push(@{$$self{fns}},
      gen('fn', {body => $self->compile($$node{body})},
        q{ sub { %@body } })) - 1;
    gen('fn ref', {index => $i}, q{ [%:index, $_[0], @_[1..$#_]] });
  },

  co  => sub { ... },
  amb => sub { ... },

  nth => sub {
    # Take a series of 
  },

  call => sub {
    # Take a [$fn_index, $parent, @args] array and tail-call into it.
    my ($self, $node) = @_;
    my @compiled = map $self->compile($_), @$node;
    gen('call', {fn_ref => $compiled[0],
                 args   => gen_array(@compiled[1..$#compiled]),
                 fns    => $$self{fns}},
      q{ ${%:fns}[%@fn_ref]->([\@_, %@args]) });
  },

  arg => sub {
    # A nontrivial thing because we may need to walk up the scope chain.
    my ($self, $node) = @_;
    gen('arg', {i     => $$node,
                deref => \&lexical_scope_deref},
      q{ %:deref->(\@_, %:i) });
  },

  global => sub {
    # Global functions don't have any closure state, so just pass in the
    # specified arguments.
    my ($self, $node) = @_;
    my $i = push(@{$$self{fns}},
                 $$self{globals}{$$node} // die "undefined global $$node") - 1;
    gen('global', {i => $i}, q{ [%:i] });
  },
);

sub compile {
  my ($self, $node, $context) = @_;
  $compilers{ref($node) =~ s/.*:://r}->($self, $node, $context // []);
}

}

DEBUG_END

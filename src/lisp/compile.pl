# ni lisp compiler
# This doesn't compile to final forms; instead, it compiles each form down to
# an internal representation that can be handed off to a JIT backend for
# optimized execution.
#
# The semantics of a ni-lisp program are dictated jointly by the space of
# values and the space of side-effects. Conceptually side-effects are modeled
# using a dependency graph that is only incidentally related to value-space;
# this means that evaluation commutes across effects unless there's a data
# dependency. To make this easier for ni to reason about, the "state of side
# effects" is treated internally as a value despite never being visible as one.
#
# You can introduce a branch-point in the side effect ordering by using the
# special form "co"; this indicates that all of its subforms are
# side-effect-commutative with respect to one another. Internally all of this
# is modeled as a doubly-linked directed dataflow graph.

{

package ni::lisp;

# Special forms:
#
# (fn* name formal body)
# (co* alternatives...)
# (do* stuff...)
# (if* value then else)
#
# Toplevel forms are implicitly wrapped in a (do*) block to provide full
# side-effect ordering.
#
# Here's how each type of form impacts ordering in general:
#
# (f x y z)      <- f, x, y, and z are unordered relative to one another
# (let* k v f)   <- v happens before f
# ((fn* ...b) x) <- x happens before b
# (do* x y z)    <- x happens before y happens before z
# (co* x y z)    <- x, y, z unordered
# (if* v t e)    <- v happens before t or e; t or e doesn't happen at all
#
# The value of a (co*) form is a list of the result of each subexpression; as a
# result, we can say this:
#
# (f x y z) = (call* (co* f x y z))
#
# In theory you could (apply* f 5) or some such, but apply* isn't provided as a
# special form. This way you can always assume a function's arguments are
# specified in a list.

sub resolve_scope {
  my ($scope, $x) = @_;
  return undef unless ref $scope eq 'HASH';
  $$scope{$x} || resolve_scope $$scope{''}, $x;
}

deftypemethod 'is_special_operator',
  list  => sub { 0 },
  array => sub { 0 },
  hash  => sub { 0 },
  qstr  => sub { 0 },
  str   => sub {
    my ($self) = @_;
    $$self if $$self eq 'fn*' || $$self eq 'let*'
           || $$self eq 'do*' || $$self eq 'co*' || $$self eq 'if*';
  },
  number => sub { 0 },
  var    => sub { 0 };

# Graph encoding
# Graphs are doubly-linked structures with directed edges indicating
# continuations. Each node represents a processing step -- possibly a no-op --
# and, if it is marked as side-effecting, constrains execution ordering.

our %special_to_graph = (
  'fn*' => sub {
    # Create a new graph link for the function's formal and self-reference and
    # add both to a new scope. We want to represent the function as a
    # disconnected graph here, adding it as a value to the surrounding graph.
    my ($scope, $self_ref, $formal, $body) = @_;
    die "fn* self ref must be a symbol (got $self_ref instead)"
      unless ref $self_ref eq 'ni::lisp::str';
    die "fn* formal must be a symbol (got $formal instead)"
      unless ref $formal eq 'ni::lisp::str';

    my $fn_graph    = fn_graph_node;
    my $inner_scope = {'' => $scope, $$formal   => $fn_graph->formal_node
                                     $$self_ref => $fn_graph};
    my $body_graph  = $body->to_graph($inner_scope);
    $$fn_graph{body} = $body_graph;
    value_node $fn_graph;
  },

  'let*' => sub {
    # This one is easy: just create a subscope that aliases the given name.
    my ($scope, $name, $value, $body) = @_;
    die "let* formal must be a symbol (got $name instead)"
      unless ref $name eq 'ni::lisp::str';
    $body->to_graph({'' => $scope, $$name => $value->to_graph($scope)});
  },

  'do*' => sub {
    # Create a series of nodes, linking each as the continuation of the
    # previous one.
    my ($scope, $first, @others) = @_;
    $first = $first->then($_) for @others;
    $first;
  },

  'co*' => sub {
    # Create a join node with large in-degree and a single output.
    my ($scope, @nodes) = @_;
    co_join_node map $_->to_graph($scope), @nodes;
  },

  'if*' => sub {
    my ($scope, $cond, $then, $else) = @_;
    if_join_node $cond->to_graph($scope),
                 $then->to_graph($scope),
                 $else->to_graph($scope);
  },

  'apply*' => sub {
    my ($scope, $f, @args) = @_;

    # Is $f a known function node (or can we resolve it to one)? If so,
    # statically reduce the form. Otherwise encode an indirect call.
    my $f_graph = $f->to_graph($scope);
    return $f_graph->call(co_join_node map $_->to_graph($scope), @args)
      if $f_graph->is_fn;

    # Otherwise co-join f and its args, and generate an indirect apply node.
    indirect_apply co_join_node $f_graph, map $_->to_graph($scope), @args;
  },
);

deftypemethod 'to_graph',
  list => sub {
    my ($self, $scope) = @_;
    my ($head, @rest) = @$self;
    my $special = $head->is_special_operator;
    $special ? $special_to_graph{$special}->($scope, @rest)
             : $special_to_graph{'apply*'}->($scope, $head, @rest);
  },

  array => sub {
    my ($self, $scope) = @_;
    array_node scalar(@$self), co_join_node map $_->to_graph($scope), @$self;
  },

  hash => sub {
    my ($self, $scope) = @_;
  },

}

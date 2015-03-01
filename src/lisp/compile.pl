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
# is modeled as a dataflow graph in which each expression is directed towards
# its continuation(s).

{

package ni::lisp;

# Special forms:
#
# (fn* name formal body)
# (let* name value body)
# (co* alternatives...)

}

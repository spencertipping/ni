# Continuation graph structure
# Represents code in a form much like CPS, but with degrees of freedom to
# encode partial ordering. Specifically:
#
# (+ (f x) (g y))
# CPS -> (λk (f x
#          (λfx (g y
#            (λgy (+ fx gy k))))))
#
# In our continuation graph structure, we replace the fixed evaluation order
# with a (co*) form that provides parallelism:
#
# (+ (f x) (g y))
# -> (λk (co* (λk1 (f x k1))
#             (λk2 (g y k2))
#             (λ[fx gy]
#               (+ fx gy k))))
#
# Semantically, (co*) returns once all of the sub-continuations are invoked,
# and additionally when any of the sub-continuations is re-invoked. So:
#
# > (co* (λk1 ...)
#        (λk2 ...)
#        (λk3 ...)
#        (λ[v1 v2 v3] (print v1 v2 v3)))
# > (k1 5)              # nothing happens
# > (k2 6)              # nothing happens
# > (k1 7)              # nothing happens
# > (k3 9)              # (print 5 6 9) (print 7 6 9)
# > (k2 4)              # (print 7 4 9)
#
# Non-triggering continuations are required to hold only a weak reference to
# the other continuation queues. That way if k3 is freed before being called,
# k1 and k2's space usage will be constant even if they are called repeatedly
# (which would normally enqueue stuff).
#
# NB: the order of arguments relative to one another is explicitly undefined;
# that is, if we have (k1 4) (k1 5) (k1 6) and (k2 a) (k2 b), the (co*)
# continuation might see [4 a] [5 a] [6 a] or it might see [4 a] [4 b] [5 b],
# etc. The queues of k1 and k2 are mutually unordered.
#
# Along with (co*) is (amb*), which forwards only the first continuation. That
# is:
#
# > (amb* (λk1 ...)
#         (λk2 ...)
#         (λv (print v)))
# > (k1 5)              # (print 5)
# > (k2 4)              # nothing happens
# > (k1 7)              # (print 7)
#
# It's important to deactivate all continuations except for the first because
# the purpose of (amb*) is to express semantic ambivalence about the
# implementation of a given computation; therefore, we still want just one
# result despite providing two ways to calculate it.

# Compiling this representation
# The graph form is reduced to special nodes, each of which is one of the
# following:
#
# (co*     f1 f2 ... k)
# (amb*    f1 f2 ... k)
# (call*   f x1 x2 ... k)
# (nth*    n x1 x2 ... xN)      # returns nth item from xs
# (fn*     [fs...] [cl...] x)   # lambda with formals and named closure refs
# (arg*    x)                   # named lexical argument
# (global* f)                   # global function named f
#
# Here's an example function and its corresponding representation:
#
# (fn [x y]
#   (print (sqrt (+ (* x x) (* y y)))))
#
# (fn* [x y k] []
#   (co* (fn* [k1] [x] (call* (global* *) (arg* x) (arg* x)))
#        (fn* [k1] [y] (call* (global* *) (arg* y) (arg* y)))
#        (fn* [v1 v2] [k]
#          (call* (global* +) (arg* v1) (arg* v2)
#            (fn* [v] [k]
#              (call* (global* sqrt) (arg* v)
#                (fn* [v] [k]
#                  (call* (global* print) (arg* v) (arg* k)))))))))
#
# In this case we can statically reclaim all memory because of the way each
# function is annotated; (*), (+), (sqrt), and (print) are each linear in their
# continuation and return values that don't alias their arguments. The ideal
# Perl compilation would look like this:
#
# sub {
#   $_[2]->(print(sqrt(($_[0]*$_[0]) + ($_[1]*$_[1]))));
# }
#
# A naive and much slower compilation would be:
#
# sub {
#   my $v1 = $_[0] * $_[0];
#   my $v2 = $_[1] * $_[1];
#   my $v3 = $v1 + $v2;
#   my $v4 = sqrt($v3);
#   my $v5 = print($v4);
#   $_[2]->($v5);
# }

# Encoding lexical scoping
# Before getting into the details, there are a few high-level constraints I'm
# dealing with in this code:
#
# 1. This is CPS-transformed source, so it could contain really deep lambdas.
# 2. This is a JIT compiler, so compilation needs to be linear-ish time.
#    Nothing quadratic in the lambda form complexity, since this will
#    compromise runtime performance.
# 3. As a side-effect of (2), if we can reuse information then we probably
#    should.
# 4. Time is more valuable than space.
#
# These performance considerations are about more than just keeping the JIT
# fast for normal cases; we also want a fast compiler so we can inline more
# inner functions to save heap allocations. The JIT can never become the
# bottleneck as we're doing this.
#
#

{

package ni::lisp::graph;

}

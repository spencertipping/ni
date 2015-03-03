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
# (nth*    i f1 f2 ... k)       # used to encode (if) and (cond)
# (fn*     n body)              # n = number of formals to bind
# (arg*    i)                   # De Bruijn index by #formals
# (global* f)                   # global function named f
#
# Here's an example function and its corresponding representation:
#
# (fn [x y]
#   (print (sqrt (+ (* x x) (* y y)))))
#
# (fn* 3                        # x y k
#   (co* (fn* 1 (call* (global* *) (arg* 1) (arg* 1) (arg* 0)))
#        (fn* 1 (call* (global* *) (arg* 2) (arg* 2) (arg* 0)))
#        (fn* 2                 # co* continuation
#          (call* (global* +) (arg* 0) (arg* 1)
#            (fn* 1
#              (call* (global* sqrt) (arg* 0)
#                (fn* 1
#                  (call* (global* print) (arg* 0) (arg* 6)))))))))
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

{

package ni::lisp::graph;

use List::Util qw/max/;

# NB: not proper constructors (call directly, not using ->fn() etc)
sub fn {
  my ($formal_vector, $body, $flags) = @_;
  $body = $body->resolve_formals(0, $formal_vector);
  bless {formals => scalar(@$formal_vector),
DEBUG
         original_formals => $formal_vector,
DEBUG_END
         body   => $body,
         degree => undef}, 'ni::lisp::graph::fn';
}

sub co     { bless \@_,            'ni::lisp::graph::co' }
sub amb    { bless \@_,            'ni::lisp::graph::amb' }
sub call   { bless \@_,            'ni::lisp::graph::call' }
sub nth    { bless \@_,            'ni::lisp::graph::nth' }
sub arg    { bless [$_[0], undef], 'ni::lisp::graph::arg' }
sub global { bless \$_[0],         'ni::lisp::graph::global' }

our %classes = qw/ co   array  amb array
                   call array  nth array /;

sub defgraphmethod {
  my ($name, %alternatives) = @_;
  *{"ni::lisp::graph::${_}::$name"} =
    $alternatives{$_} // $classes{$_} && $alternatives{$classes{$_}}
                      // sub { $_[0] }
    for keys %alternatives;
}

defgraphmethod 'degree',
  fn     => sub {
    my ($self) = @_;
    $$self{degree} //= $$self{body}->degree - $$self{formals};
  },
  array  => sub { my ($self) = @_; max map $_->degree, @$self },
  arg    => sub { my ($self) = @_; $$self[1] // die "unlinked arg: $$self[0]" },
  global => sub { 0 };

defgraphmethod 'resolve_formals',
  fn => sub {
    my ($self, $offset, $formals) = @_;
    $$self{body}->resolve_formals($offset + $$self{formals}, $formals);
    $self;
  },
  array => sub {
    my ($self, $offset, $formals) = @_;
    $_->resolve_formals($offset, $formals) for @$self;
    $self;
  },
  arg => sub {
    my ($self, $offset, $formals) = @_;
    return $self if defined $$self[1];
    for (0 .. $#{$formals}) {
      if ($$self[0] eq $$formals[$_]) {
        $$self[1] = $_;
        return $self;
      }
    }
    $self;
  };

DEBUG

sub array_str_fn {
  my ($header) = @_;
  sub {
    my ($self) = @_;
    "($header" . join('', map " " . $_->str, @$self) . ")";
  };
}

defgraphmethod 'str',
  fn => sub {
    my ($self) = @_;
    "(fn* $$self{formals} [@$$self{original_formals}] "
      . $$self{body}->str . ")";
  },
  co   => array_str_fn('co*'),
  amb  => array_str_fn('amb*'),
  call => array_str_fn('call*'),
  nth  => array_str_fn('nth*'),
  arg  => sub {
    my ($self) = @_;
    "(arg* $$self[0] $$self[1])";
  },
  global => sub {
    my ($self) = @_;
    "(global* $$self)";
  };

DEBUG_END

}

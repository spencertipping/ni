# Bootstrap ni-lisp to concatenative macroexpander-ish thing
#
# Backstory here:
# ni-lisp is compiled into concatenative form by a continuation transformer
# that is itself a concatenative function. In that sense it's self-hosting; the
# continuation transformer is a defined and quotable quantity that can then be
# translated into any backend-supported language. This provides the ability to
# launch a repl from any backend.
#
# The problem is that writing one of these things in concatenative form is
# miserable, so I'm cheating a little and writing the
# applicative-to-concatenative compiler twice: once here in Perl and later in
# ni-lisp. That way I can treat concatenative as a purely intermediate
# representation.
#
# Calling convention.
# When we have a ni-lisp function like this:
#
# (fn [x] (inc x))
#
# we somehow need to map its arguments and return value(s) into the
# concatenative model. It's tempting to say that each argument and return value
# occupies a stack cell, but this requires that we know the (fixed) arity of
# each function up front. The ultimate goal is to produce code that uses the
# stack in very predictable ways so no stack is involved in the final
# dynamically-compiled version.
#
# So instead of doing it that way, each argument list and return is represented
# as a single value of some kind; that is, all functions are internally unary.
#
# Closure compilation.
# Closures are just regular functions that contain quotations of values. For
# example:
#
# (fn [x] (fn [y] x))
#
# Lexical variables are statically resolved, so the above function compiles
# something like this:
#
# (                             # (x)
#   unswons drop quote          # 'x
#   () swons                    # ('x)
#   'drop cons                  # (drop 'x)
# )
#
# Special forms.
# This is more interesting than it sounds because ni-lisp can't refer to any
# concatenative functions directly (due to the difference in calling
# conventions). So concatenative functions like (co*) and (amb*) get their own
# special forms. A convenient consequence of this is that you can't alias
# either one, which is good because (1) you'd never need to, and (2) if you did
# it would make macroexpansion-as-analysis nearly impossible.
#
# Having said all of that, here's the list of special forms ni-lisp recognizes:
#
# (fn* self formal body)        # unary formal and return (!)
# (co* f1 f2 ... fN)            # returns a list of ((f1) (f2) ... (fN))
# (amb* f1 f2 ... fN)           # returns one of (f1), (f2), ..., or (fN)
# (nth* i x0 x1 ... xN)         # does + returns x_i, segfaults if OOB
# (do* x1 x2 ... xN)            # do x1, then x2, ..., then do + return xN
#
# We also allocate special forms to do stuff with continuations and resolvers:
#
# (def* name value)             # updates the resolver
# (call/cc* f)                  # calls f with the current continuation
#
# (def*) normally compiles its value, so you can say (def* foo (fn [x] x)) and
# expect that to do the right thing (given a suitable macro for fn). However,
# there may be times when you want to define a function using concatenative
# primitives; to do that, you can use the fact that lists are functionals:
#
# (def* foo '(5 swons))
# (foo 1 2 3 4)                 # -> (5 1 2 3 4)
#
# Continuations are inspectable lists and ni-lisp functions, but they contain
# some wrapper code to convert from the concatenative notion to functions that
# behave like the ones in Scheme. (That is, ni-lisp isn't itself CPS-converted;
# its continuations are internally encoded as concatenative continuations,
# which makes them fully serializable.)
#
# If you use call/cc*, the continuation you receive will always have the
# following form:
#
# (                     # continuation args should be in a list on the stack
#   (data-stack)        # self-quoting list form
#   swons               # function call to concatenative "swons"
#   (tail-stack)        # self-quoting list form
#   'resolver           # self-quoting form of some kind (probably a hash)
#   setcc               # function call to concatenative "setcc"
# )
#
# This means you can reliably inspect and transform the continuation (which is
# how ni-lisp implements low-level fexprs, though you shouldn't use them).
# Generating and using a continuation is often much faster than inspecting it.
#
# The resolver's role in ni-lisp is a little strange. ni-lisp uses a wrapped
# calling convention, so it requires a separate namespace. This is done by
# having all ni-lisp names internally prefixed with :. Typically you wouldn't
# encounter this prefix unless you were printing a function as a list or some
# such.
#
# Interfacing functions.
# In addition to the above special forms, we need a set of builtin functions so
# we can do stuff with data structures. These are intentionally named after
# functions in Clojure and have similar semantics:
#
# (symbol? x)                   # predicates return 0 or 1
# (string? x)                   # (booleans are for wimps)
# (number? x)
# (nil? xs)
# (count xs)
# (tag xs)                      # returns symbol prefix (see below)
# (with-tag xs t)               # changes symbol prefix
#
# (cons x xs)                   # basis for (list-conj)
# (first xs)
# (rest xs)
# (list x y z ...)
# (list? x)
#
# (vector a b c ...)
# (vec xs)
# (nth xs i)
# (vec? x)
# (vec-conj xs x)               # generalized conj is defined later
#
# (hash-map k1 v1 k2 v2 ...)
# (hash-conj xs [k v])          # basis for (assoc)
# (hash-unconj h k)             # basis for (dissoc)
# (keys h)
# (vals h)
# (contains? h k)
# (get h k)
# (get h k not-found)
# (hash? x)                     # later aliased as (map?)
#
# Other data structures are built on top of these "primitives" to (mostly)
# achieve parity with Clojure.
#
# Vectors and maps can be prefixed with a symbol that doesn't change the
# semantics of any primitive operators, but does remain with the data structure
# across changes. For example:
#
# #[1 2 3]                      # a hashset of numbers
# point{:x 3 :y 4}              # a named map
# (assoc p{} :x 3)              # -> p{:x 3}
# (tag point{:x 3 :y 4})        # -> 'point
#
# You can use this tag to implement derivative data structures based on the
# builtin ones.

package nb;

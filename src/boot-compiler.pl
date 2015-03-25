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
# as a single value of some kind. It doesn't matter whether it's a list, array,
# or anything else, though most of the time it will be. This paradigm needs to
# be able to represent everything we'll ever want to do, so it requires a list
# for return values despite most functions returning exactly one. (Almost all
# of these intermediate structures are erased by abstract evaluation and
# therefore never allocated.)
#
# A function like (fn [x] (inc x)), then, is compiled to take a list of the
# form (x) and return a list of the form (y).
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
# (                     # (x)
#   unswons drop        # x
#   () swons () swons   # ((x))
#   'drop cons          # (drop (x))
#   () swons            # ((drop (x)))
# )
#
# It won't end up making any difference, but in this example it's fine to have
# the inner function not construct any lists (it just contains a list literal,
# which will push a reference to itself) because lists are immutable. That
# said, it won't matter because by the time any code hits a backend the return
# list will have been erased.

package nb;

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
# (fn* formal body)             # unary formal and return (!)
# (co* f1 f2 ... fN)            # returns a list of ((f1) (f2) ... (fN))
# (amb* f1 f2 ... fN)           # returns one of (f1), (f2), ..., or (fN)
# (nth* i x0 x1 ... xN)         # returns x_i, segfaults if i out of bounds
# (do* x1 x2 ... xN)            # do x1, x2, ..., xN sequentially, return xN
#
# We also allocate special forms to do stuff with continuations and resolvers:
#
# (def* name value)             # updates the resolver
# (call/cc* f)                  # calls f with an inspectable continuation
# (set/cc* k)                   # uses a continuation
#
# Continuations behave a little differently than they do in Scheme in that you
# can't just invoke one as a function. However, despite this limitation they
# have the same performance and space characteristics (i.e. amortized constant
# time, and minimal amortized space).
#
# The resolver's role in ni-lisp is a little strange. ni-lisp uses a wrapped
# calling convention, so it requires a separate namespace. This is encoded by
# having all ni-lisp names internally prefixed with :. Typically you wouldn't
# encounter this prefix unless you were printing a function as a list or some
# such.

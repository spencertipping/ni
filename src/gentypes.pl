# Type-tracked code fragments
# ni data functions typically receive a list of arguments corresponding to
# tab-delimited values, but file data is read line by line. We could convert
# every function's output to a line string and reparse for the next function,
# but this is quite slow and unnecessary. Instead, we track the type of each
# code element and do conversions only when appropriate.
#
# This model gives us the ability to optimize a bit further: calling
# split(/\t/) is actually quite slow compared to pulling fields out using
# index() and relying on substr() to do aliasing. So at this point we have two
# possible representations:
#
# 1. Fields stored in @_, which is what we'll have after a function call
# 2. Field positions stored in index variables, accessed using substr()
#
# We aren't done quite yet. We also sometimes have situations where we're
# dealing with flatmapped output from a function, and we want to be able to
# return multiple rows each with multiple columns. Perl references are a
# performance disaster, so we instead pack everything into a single array with
# bundles prefixed by length:
#
# (3, 'foo', 'bar', 'bif', 1, 'bif')  ->  foo   bar   bif
#                                         baz
#
# This gives us a third representation, packed-arrays.
#
# There's still more we can do. In some cases we can tell which fields are
# accessed by a function. If the function supplying data generates more fields
# (particularly in the packed-array case), we can sometimes modify it to return
# fewer values. This matters because Perl copies strings when you move them
# between arrays. So, for example, if we have a situation like this:
#
# flatmap sub { (row(a, b, c), row(d, e, f)) }
# map     sub { %0 + %1 }
#
# we don't actually need to emit a packed array at all; the row() function can
# contain the map code and the resulting type ends up being a series of single
# values. The flatmap function must end up calling row() since otherwise it
# would have no way to emit a packed array in the first place.
#
# Perl code can't be type-inferred beyond doing something simple like scanning
# for various field references. Even if we could use a combination of operator
# overloading and CORE shadowing to get a symbolic form of some chunk of code
# (which is tempting), we'd run into problems anytime that code made any call
# to something external -- particularly native code.
#
# Given that, we can make the following assumptions:
#
# 1. We can find the set of fields a function uses, though not what it does
#    with them. Sometimes a function will use the field array, but we'll know
#    this too.
# 2. In some (known) cases, the function must use some explicit return function
#    like row() -- and we'll need to be able to rewrite this with our own
#    custom function.

# TODO

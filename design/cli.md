# Command-line syntax
ni's command-line syntax needs to be right-concatenative for usability reasons:
the edit point is always on the end as you revise a command, so the evaluator
needs to go from left to right. However, this obviously doesn't work correctly
at the micro-level:

```sh
$ ni /usr/share/dict/words t100 D[m1:g m2:g]
#    --------------------> ---> ----------->
#       append to empty    take  distribute
```

The overall flow is left to right, but each command's arguments _follow_ it
rather than preceding it. Continuation-parsing grammars don't help here because
the unevaluated continuation has undefined arity.

## Two-stage evaluation
We have this problem in the first place just because Canard's strictly
rightwards evaluation model doesn't suit the subject-verb-object ordering often
used for command-line arguments. The solution is to evaluate the command line
in two stages before stream assembly:

1. Evaluate metaprogramming from left to right
2. Evaluate operator syntax from right to left
3. Evaluate operators themselves from left to right (stream assembly)

This is ideal because it also addresses command shorthands as consequences of
name resolution, and because it means that all operators are just regular
Canard functions that take arguments and return stream-transforming functions.
This, in turn, makes it possible to define `explain` functionality.

**NB:** This design requires operators and metaprogramming functions to have
disjoint namespaces.

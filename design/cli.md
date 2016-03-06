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
the unevaluated continuation has undefined arity. We solve this problem by
having Canard evaluate from right to left, but having the operators themselves
produce a list of stream-modifying functions that is then applied in reverse:

```sh
$ ni %. [/usr/share/dict/words]     # metaprogramming evaluation (step 1)
#    <- <----------------------     #
#    |        push list             #
#    eval                           #

$ ni /usr/share/dict/words          # metaprogramming evaluation (step 2)
#    ---------------------          #
#        resolve name               #

[empty-stream                       # result of metaprogramming evaluation
 [append file                       # <- operation on empty-stream
         "/usr/share/dict/words"]]  #
```

Now the list is executed from left to right:

```
empty-stream                            -> a stream
[append file "/usr/share/dict/words"]   -> eval sublist right to left
```

This evaluation model means we might as well have typed this and evaluated
right to left:

```
append file "/usr/share/dict/words" empty-stream
```

And that is exactly what we need.

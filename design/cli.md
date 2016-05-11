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

## Huge problem alert (but maybe not; see below)
Suppose we've got this:

```sh
$ ni /usr/share/dict/words X 'grep foo'
```

The canard reader/evaluator goes from right to left and will behave like this:

```
grep foo                -> "g" followed by "rep foo"
                        -> group operator; "rep foo" isn't a groupspec
                        -> "r" followed by "ep foo"
                        -> ...
X                       -> oops, "grep foo" should have been a command arg
                        -> we screwed up because spencer designed this crappily
                        -> aaaaargh
```

Commands and arguments need to be identifiable _syntactically_ if we want
juxtaposed short commands. I think this means we need a `-` prefix,
unfortunately, so anything else becomes self-quoting. Unfortunate but
unavoidable.

### Possible resolution to this
In the [CLI reader problem note](cli-reader-problem.md) I decided that ni needs
to resolve symbols at parse-time for a forward-syntax pass. This makes it
possible to have quoted code that isn't shell-separated from canard brackets,
and to ask the interpreter whether something can be parsed.

This mechanism already parameterizes the reader on symbol resolution, so we can
do the same to its quotation inference. This allows us to remove prefix dashes
in most cases (as long there's no way to misinterpret operators as quasifiles).
So the following would be unambiguous:

```sh
$ ni foo t40d1D[machine1:[om'r a + b'] ...]
```

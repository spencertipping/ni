# ni's implementation of canard
[Canard](https://github.com/spencertipping/canard/tree/master) is more of an
idea than it is a specific programming language. It ends up giving you a very
powerful metacircular base language with minimal implementation effort, though,
and that's ideal for ni. You could read the documentation for the canard
project, but I'll go ahead and describe everything here since there are
differences.

Canard is a concatenative quasilisp very similar to
[Joy](https://en.wikipedia.org/wiki/Joy_(programming_language)) in its
fundamental principles of operation, but its implementation has more in common
with [jonesforth](http://git.annexia.org/?p=jonesforth.git;a=summary). It also
has some oddities like right-to-left evaluation, a very flexible symbol
resolution mechanism, and "backwards" list consing.

Superficially, you can expect canard to behave like Joy or Forth in reverse:

```
+ 3 4           -> 7
+ + 3 4 5       -> 12
:: [] 4         -> [4]          (:: is cons)
:^ [4]          -> [] 4         (:^ is uncons)
* [+ 1] [1 2 3] -> [2 3 4]      (* is map)
```

- [How the interpreter works](interpreter.md)
- [How the language is bootstrapped](bootstrap.md)

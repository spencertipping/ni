# ni lisp
ni implements a Lisp-style programming language designed jointly to be familiar
to Clojure programmers and to be easily optimizable for IO tasks. Most basic
Clojure idioms will work like you expect:

```clj
> (map inc [1 2 3])
[2 3 4]                 # minor difference; Clojure gives (2 3 4)
> (reduce + 0 (range 5))
10
> (:foo {:foo 5})
5
> (defn f [x] (str x))
#'f
> (f [5 6])
"[5 6]"
```

The major differences between ni-lisp and Clojure are:

- All ni-lisp values are immutable and losslessly serializable
- ni-lisp does not support direct interop with any of its backends
- ni-lisp provides concurrency primitives
- ni-lisp provides first-class reusable continuations with fast `call/cc`
- ni-lisp's quotation and CPS-conversion are implemented as macros

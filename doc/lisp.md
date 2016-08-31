# Common Lisp driver
ni supports Common Lisp via SBCL, which is available using the `l` and `L`
operators. For example:

```bash
$ ni n:4l'(+ a 2)'
3
4
5
6
```

## Basic stuff
`a` to `q` are one-letter functions that return the first 17 tab-delimited
values from the current line. `(r ...)` is a function that takes a list of
values and prints a tab-delimited row. For example:

```bash
$ ni n:4l'(r a (1+ a))'                   # generate two columns
1	2
2	3
3	4
4	5
$ ni n:4l'(r a (1+ a))' l'(r (+ a b))'        # ... and sum them
3
5
7
9
```

Note that whitespace is required after every `p'code'` operator; otherwise ni
will assume that everything following your quoted code is also Perl.

It is possible to omit `r` altogether; then you're returning one or more
values, each of which will become a row of output:

```bash
$ ni n:2l'a (+ a 100)'                   # return without "r"
1
101
2
102
```

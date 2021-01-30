# Common Lisp driver
ni supports Common Lisp via SBCL, which is available using the `l` and `L`
operators. For example:

```bash
$ ni n4l'(+ a 2)'
3
4
5
6
```

If you don't have SBCL installed locally, you can use the `C` (containerize)
operator to run a Docker image:

```lazytest
if ! [[ -e /nodocker ]]; then
```

```bash
$ docker build -q -t ni-test/sbcl - <<EOF > /dev/null
FROM ubuntu
RUN apt-get update
RUN apt-get install -y sbcl
CMD /bin/bash
EOF
$ ni Cni-test/sbcl[n4l'(+ a 2)']
3
4
5
6
```

```lazytest
fi                      # -e /nodocker
```

## Basic stuff
`a` to `q` are one-letter functions that return the first 17 tab-delimited
values from the current line. `(r ...)` is a function that takes a list of
values and prints a tab-delimited row. For example:

```bash
$ ni n4l'(r a (1+ a))'                  # generate two columns
1	2
2	3
3	4
4	5
$ ni n4l'(r a (1+ a))' l'(r (+ a b))'   # ... and sum them
3
5
7
9
```

Note that whitespace is required after every `l'code'` operator; otherwise ni
will assume that everything following your quoted code is also Lisp.

It is possible to omit `r` altogether; then you're returning one or more
values, each of which will become a row of output:

```bash
$ ni n2l'a (+ a 100)'                   # return without "r"
1
101
2
102
```

## Streaming lookahead
This is implemented in terms of reducers, and gives you the ability to reduce
arbitrarily many rows in constant space. There are two parts to this. First,
the streaming reduce functions `se` and `sr`; and second, compound reducers
(very useful, and explained in the next section).

### `sr`
Reduces the entire data stream:

```
sr (reducer value [initial-value])* => reduced-value*
```

For example, to sum arbitrarily many numbers in constant space:

```bash
$ ni n10000l"(sr ('+ a))"
50005000
```

Or to reduce multiple columns into a row:

```bash
$ ni n4fAA l"(r (sr ('+ a) ('* b)))"
10	24
```

### `se`
Reduces over a contiguous group of rows for which the partition function
remains equal. (Mnemonic is "stream while equal".)

```
se function value-form partition-form [initial-value] => reduced-value
```

For example, to naively get a comma-delimited list of users by login shell:

```bash
$ ni /etc/passwd F::gG l"(r g (se (partial #'join #\,) a g))"
/bin/bash	root
/bin/false	syslog
/bin/sh	backup,bin,daemon,games,gnats,irc,libuuid,list,lp,mail,man,news,nobody,proxy,sys,uucp,www-data
/bin/sync	sync
```

# Python interface
ni provides the `y` operator to execute a Python line processor on the current
data stream. For example:

```bash
$ ni n5y'int(a) * int(a)'                 # square some numbers
1
4
9
16
25
```

`y` does a decent job of figuring out where a chunk of code ends where lambdas
are concerned:

```bash
$ ni ::pyfoo[n4p'int(a)*int(a)'] //:pyfoo
1
4
9
16
```


## Basic stuff
`a` to `l` are one-letter variables that return the first 12 tab-delimited
values from the current line. `r(*xs)` is a function that takes a list of values
and prints a tab-delimited row. For example:

```bash
$ ni n4y'r(a, int(a) + 1)'                        # generate two columns
1	2
2	3
3	4
4	5
$ ni n4y'r(a, int(a) + 1)' y'r(int(a) + int(b))'  # ... and sum them
3
5
7
9
```

Note that whitespace is required after every `y'code'` operator; otherwise ni
will assume that everything following your quoted code is also Python.


### `F`: the array of fields
The Python code given to `y` is invoked on each line of input, which is stored
in `_`. `F` is a list of fields split on tabs:

```bash
$ ni /etc/passwd F::r3
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
bin	x	2	2	bin	/bin	/bin/sh
$ ni /etc/passwd F::r3y'F[0:4]'
root	x	0	0
daemon	x	1	1
bin	x	2	2
$ ni /etc/passwd F::r3y'F[1:4]'
x	0	0
x	1	1
x	2	2
$ ni /etc/passwd F::r3y'len(F)'         # number of fields
7
7
7
```


## Multiline Python code
ni will figure out how to indent your code if you span multiple lines. For
example:

```bash
$ ni n3 y'x = int(a)
          r(x, x + 1)'
1	2
2	3
3	4
$ ni n3 y'for i in range(int(a)):
            r(i)'
0
0
1
0
1
2
```


## `^:`: setup blocks
You can designate some code to be run once before the first input line, as
opposed to for every input line in the stream. This is a setup block, written
at the beginning of a Python code argument as `^:`:

```bash
$ ni n3y'^:
           self.x = 10
         self.x += 1
         r(self.x)'
11
12
13
```

Note that variables must be stored on `self` to live beyond a single iteration.

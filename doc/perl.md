# Perl interface
**NOTE:** This documentation covers ni's Perl data transformer, not the
internal libraries you use to extend ni. For the latter, see
[extend.md](extend.md) (`ni //help/extend`).

ni provides the `p` operator to execute a Perl line processor on the current
data stream. For example:

```bash
$ ni n:5p'a * a'                # square some numbers
1
4
9
16
25
```

## Basic stuff
`a` to `q` are one-letter functions that return the first 17 tab-delimited
values from the current line. `r(...)` is a function that takes a list of
values and prints a tab-delimited row. For example:

```bash
$ ni n:4p'r a, a + 1'                   # generate two columns
1	2
2	3
3	4
4	5
$ ni n:4p'r a, a + 1' p'r a + b'        # ... and sum them
3
5
7
9
```

Note that whitespace is required after every `p'code'` operator; otherwise ni
will assume that everything following your quoted code is also Perl.

Internally, `a`, `b`, etc are defined like this, which I explain below:

```pl
sub a() {F_ 0}
sub b() {F_ 1}
...
```

### `F_`: the array of fields
The Perl code given to `p` is invoked on each line of input, which is stored
both in `$l` and, for convenience, in `$_`. ni doesn't split `$l` into fields
until you call `F_`, at which point the split happens and the fields are
cached for efficiency.

`F_(...)` takes one or more column indexes (as zero-based integers) and returns
the field values. If you don't pass in anything, it returns all of the fields
for the line. For example:

```bash
$ ni /etc/passwd F::r3
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
bin	x	2	2	bin	/bin	/bin/sh
$ ni /etc/passwd F::r3p'r F_ 0..3'
root	x	0	0
daemon	x	1	1
bin	x	2	2
$ ni /etc/passwd F::r3p'r scalar F_'            # number of fields
7
7
7
```

### `r`, multiple rows, and return values
`p` executes your Perl code using essentially this template:

```pl
sub row {
  # your code goes here
}
while (<STDIN>) {
  $l = $_;
  print "$_\n" for row();
}
```

Counterintuitively, `r(...)` doesn't return a value; it returns an empty list
and side-effectfully prints a row. It works this way because that allows you to
generate arbitrarily many rows in constant space.

This design is also what makes it possible to omit `r` altogether; then you're
returning one or more values, each of which will become a row of output:

```bash
$ ni n:2p'a, a + 100'                   # return without "r"
1
101
2
102
$ ni n:2p'r a, a + 100'                 # use "r" for side effect, return ()
1	101
2	102
$ ni n:3p'r $_ for 1..a; ()'            # use r imperatively, explicit return
1
1
2
1
2
3
$ ni n:3p'r $_ for 1..a'                # use r imperatively, implicit return
1

1
2

1
2
3
```

The last example has blank lines because Perl's `for` construct returns a
single empty scalar. You can suppress any implicit returns using `;()` at the
end of your mapper code.

Whether you use `r` or implicit returns, ni will remove newlines from every
string you give it. This makes it easier to use `qx` without any filtering.

## Utility functions
ni predefines a bunch of useful functions that various versions of Perl may or
may not provide by default:

- `sr(match, replace, str)`: equivalent to `str =~ s/match/replace/r`, but
  works prior to when Perl's `/r` flag was introduced
- `sgr(match, replace, str)`: `str =~ s/match/replace/gr`

- `sum(@)`, `prod(@)`, `mean(@)`
- `max(@)`, `min(@)`, `maxstr(@)`, `minstr(@)`, `argmax(&@)`, `argmin(&@)`
- `any(&@)`, `all(&@)`, `uniq(@)`, `%f = %{freqs(@)}`
- `reduce {f} $init, @xs`
- `reductions {f} $init, @xs`
- `cart([a1, a2, a3], [b1, b2, b3], ...) = [a1, b1, ...], [a1, b2, ...], ...`:
  Cartesian product

## Aggregation
`p` code can read forwards in the input stream. This is trivially possible by
calling `rl()` ("read line"), which destructively advances to the next line and
returns it; but more likely you'd use one of these instead:

- `@lines = rw {condition}`: read lines while a condition is met
- `@lines = ru {condition}`: read lines until a condition is met
- `@lines = re {a + b}`: read lines while `a + b` is equal

**NOTE:** `rw`, `ru`, and `re` are destructive operators in that they consume
lines and destroy the context for `a`, `b`, etc.

```bash
$ ni n:10p'r ru {a%4 == 0}'             # read forward until a multiple of 4
1	2	3
4	5	6	7
8	9	10
```

The line array returned by `ru` is just an array of flat, tab-delimited strings
(verbatim lines from standard input), but you can extract fields using the
column-accessor functions `a_`, `b_`, etc:

```bash
$ ni n:10p'r map a*$_, 1..10' | tee mult-table
1	2	3	4	5	6	7	8	9	10
2	4	6	8	10	12	14	16	18	20
3	6	9	12	15	18	21	24	27	30
4	8	12	16	20	24	28	32	36	40
5	10	15	20	25	30	35	40	45	50
6	12	18	24	30	36	42	48	54	60
7	14	21	28	35	42	49	56	63	70
8	16	24	32	40	48	56	64	72	80
9	18	27	36	45	54	63	72	81	90
10	20	30	40	50	60	70	80	90	100
$ ni mult-table p'r g_ ru {a%4 == 0}'   # extract seventh column from each line
7	14	21
28	35	42	49
56	63	70
```

`a_` etc are defined like this:

```pl
sub a_ {local $_; map((split /\t/)[0], @_)}
sub b_ {local $_; map((split /\t/)[1], @_)}
...
```

## Facet aggregation


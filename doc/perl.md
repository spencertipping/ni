# Perl interface
**NOTE:** This documentation covers ni's Perl data transformer, not the
internal libraries you use to extend ni. For the latter, see
[extend.md](extend.md) (`ni //help/extend`).

For a data transformer you're more likely to use and like, see
[ruby.md](ruby.md).

ni provides the `p` operator to execute a Perl line processor on the current
data stream. For example:

```bash
$ ni n5p'a * a'                 # square some numbers
1
4
9
16
25
```

`p` does a decent job of figuring out where a chunk of code ends where lambdas
are concerned:

```bash
$ ni ::plfoo[n4p'a*a'] //:plfoo
1
4
9
16
```

**NOTE:** The Perl driver won't execute your code at all if the input stream is
empty. If you're using the Perl driver to generate data, you need to feed it a
row using, e.g. `n1`:

```bash
$ ni +p'1..5'                   # nothing happens here
$ ni +n1p'1..5'                 # the single input row causes `p` to run
1
2
3
4
5
```

## Basic stuff
`a` to `l` are one-letter functions that return the first 12 tab-delimited
values from the current line. (12 wasn't chosen arbitrarily; the letter `m` is
Perl syntax for regular expressions, so we can't use it.) `r(...)` is a
function that takes a list of values and prints a tab-delimited row. For
example:

```bash
$ ni n4p'r a, a + 1'                    # generate two columns
1	2
2	3
3	4
4	5
$ ni n4p'r a, a + 1' p'r a + b'         # ... and sum them
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
The Perl code given to `p` is invoked on each line of input, which is stored in `$_`. 
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
$ ni /etc/passwd F::r3p'r F_ 1..3'
x	0	0
x	1	1
x	2	2
$ ni /etc/passwd F::r3p'r scalar F_'            # number of fields
7
7
7
```

If you want to select field ranges to the end of the line (like Perl's
`@_[1..$#_]` construct), `FM` is analogous to `$#_`:

```bash
$ ni /etc/passwd F::r3p'r F_ 3..FM'
0	root	/root	/bin/bash
1	daemon	/usr/sbin	/bin/sh
2	bin	/bin	/bin/sh
$ ni /etc/passwd F::r3p'r FR 3'         # FR(n) == F_(n..FM)
0	root	/root	/bin/bash
1	daemon	/usr/sbin	/bin/sh
2	bin	/bin	/bin/sh
```

### `r`, multiple rows, and return values
`p` executes your Perl code using essentially this template:

```pl
sub row {
  # your code goes here
}
while (<STDIN>) {
  defined $_ && print "$_\n" for row();
}
```

Counterintuitively, `r(...)` doesn't return a value; it returns an empty list
and side-effectfully prints a row. It works this way because that allows you to
generate arbitrarily many rows in constant space.

This design is also what makes it possible to omit `r` altogether; then you're
returning one or more values, each of which will become a row of output:

```bash
$ ni n2p'a, a + 100'                    # return without "r"
1
101
2
102
$ ni n2p'r a, a + 100'                  # use "r" for side effect, return ()
1	101
2	102
$ ni n3p'r $_ for 1..a'                 # use r imperatively, implicit return
1
1
2
1
2
3
```

As a shorthand, any array references you return will become rows:

```bash
$ ni n3p'[a, a+1, a+2]'
1	2	3
2	3	4
3	4	5
```

## Buffered readahead
`p` code can read forwards in the input stream. This is trivially possible by
calling `rl()` ("read line"), which destructively advances to the next line and
returns it; but more likely you'd use one of these instead:

- `@lines = rw {condition}`: read lines while a condition is met
- `@lines = ru {condition}`: read lines until a condition is met
- `@lines = re {a + b}`: read lines while `a + b` is equal

**NOTE:** `rw`, `ru`, and `re` are destructive operators in that they consume
lines and destroy the context for `a`, `b`, etc.

```bash
$ ni n10p'r ru {a%4 == 0}'              # read forward until a multiple of 4
1	2	3
4	5	6	7
8	9	10
```

The line array returned by `ru` is just an array of flat, tab-delimited strings
(verbatim lines from standard input), but you can extract fields using the
column-accessor functions `a_`, `b_`, etc:

```bash
$ ni n10p'r map a*$_, 1..10' =\>mult-table
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

`a_` etc are defined like this, with an exception for the scalar return case:

```pl
sub a_ {local $_; map((split /\t/)[0], map split(/\n/), @_)}
sub b_ {local $_; map((split /\t/)[1], map split(/\n/), @_)}
...
```

The line split enables more idiomatic handling of data closures:

```bash
$ ni ::squares[n100p'100 - a' p'r a, a*a'] \
     n5p'^{@sq{a_ squares} = b_ squares} $sq{a()}'
1
4
9
16
25
```

## Hash constructors
The above code can be condensed a bit with hash constructors, which are pairs
of columns:

```bash
$ ni ::squares[n100p'100 - a' p'r a, a*a'] n5p'^{%sq = ab_ squares} $sq{a()}'
1
4
9
16
25
```

## Utility functions
ni predefines some stuff you may find useful:

- `sum(@)`, `prod(@)`, `mean(@)`
- `max(@)`, `min(@)`, `maxstr(@)`, `minstr(@)`, `argmax(&@)`, `argmin(&@)`
- `any(&@)`, `all(&@)`, `uniq(@)`, `%f = %{freqs(@)}`
- `reduce {f} $init, @xs`
- `reductions {f} $init, @xs`
- `cart([a1, a2, a3], [b1, b2, b3], ...) = [a1, b1, ...], [a1, b2, ...], ...`:
  Cartesian product

```bash
$ ni n100p'sum rw {1}'
5050
$ ni n10p'prod rw {1}'
3628800
$ ni n100p'mean rw {1}'
50.5
```

The Cartesian product function works, which is a considerable improvement over
its prior behavior:

```bash
$ ni n1p'cart [1,2], [1,2,3], ["a","b"]'
1	1	a
2	1	a
1	2	a
2	2	a
1	3	a
2	3	a
1	1	b
2	1	b
1	2	b
2	2	b
1	3	b
2	3	b
```

## Streaming lookahead
This is implemented in terms of reducers, and gives you the ability to reduce
arbitrarily many rows in constant space. There are two parts to this. First,
the streaming reduce functions `se` and `sr`; and second, compound reducers
(very useful, and explained in the next section).

### `sr`
Reduces the entire data stream:

```pl
@final_state = sr {reducer} @init_state
```

For example, to sum arbitrarily many numbers in constant space:

```bash
$ ni n10000p'sr {$_[0] + a} 0'
50005000
```

### `se`
Reduces over a contiguous group of rows for which the partition function
remains equal. (Mnemonic is "stream while equal".)

```pl
@final_state = se {reducer} \&partition_fn, @init_state
```

**NOTE**: There is _no comma_ between the reducer and the partition function.


For example, to naively get a comma-delimited list of users by login shell:

```bash
$ ni /etc/passwd F::gGp'r g, se {"$_[0]," . a} \&g, ""'
/bin/bash	,root
/bin/false	,syslog
/bin/sh	,backup,bin,daemon,games,gnats,irc,libuuid,list,lp,mail,man,news,nobody,proxy,sys,uucp,www-data
/bin/sync	,sync
```

`se` has shorthands for the first 17 columns: `sea`, `seb`, ..., `seq`; they are called as:

```pl
@final_state = sea {reducer} @init_state
```


## Compound reducers
If you want to do something like calculating the sum of one column, the average
of another one, and the min/max of a third, you'll end up writing some awkward
reducer code. ni provides a facility called compound reduction to deal with
this. For example, here's the hard way:

```bash
$ ni n100p'my ($sum, $n, $min, $max) = sr {$_[0] + a, $_[1] + 1,
                                            min($_[2], a), max($_[2], a)}
                                           0, 0, a, a;
            r $sum, $sum / $n, $min, $max'
5050	50.5	1	100
```

And here's the easy way, using `rc`:

```bash
$ ni n100p'r rc \&sr, rsum "a", rmean "a", rmin "a", rmax "a"'
5050	50.5	1	100
```

### What's going on here
`rsum`, `rmean`, etc, return compound reducers, which are hash references with
keys that indicate (1) the initial state, (2) the reduction function **as a
string**, and (3) the finalizer (which is why `rmean` can return a single
number despite its intermediate state being the separated sum and number).

Compound reducers are compiled functions, which means their arguments are
expressed as strings representing quoted code. This is why we use `"a"` rather
than `a` in the example above: the string `'a'` is spliced into a function body
along with the other reducer expressions and compiled. The result is a very
efficient reducer function that ends up looking like this:

```pl
sub {($_[0] + a, $_[1] + a, $_[2] + 1, min($_[3], a), max($_[4], a))}
```

`rc` hands this function to `sr` and then uses the finalizer functions to
return individual values, one per initial reducer.

### Custom compound reducers
You can easily create your own compound reducer using `rfn`. For example, let's
count the frequency of each lowercase letter in a file:

```bash
$ ni /etc/passwd FWpsplit// r/[a-z]/ \
     p'my %freqs = %{rc \&sr, rfn q{ ++${%1}{a()} && %1 }, {}};
       map r($_, $freqs{$_}), sort keys %freqs'
a	39
b	36
c	14
d	13
e	17
f	1
g	11
h	20
i	46
k	3
l	19
m	14
n	50
o	25
p	15
r	24
s	51
t	15
u	17
v	12
w	12
x	23
y	12
```

Here's what's going on.

- `FW`: split into words
- `psplit//`: the same as `p'split //'`: split the line into individual chars,
  returned as an array so we end up with each on its own output line
- `r/[a-z]/`: keep all lowercase letters from those rows
- `p'...'`: apply perl to rows
  - `rfn q{ ++${%1}{a()} && %1 }, {}`: a reducer whose initial state is the
    empty hash `{}`, and whose reducer function does the following:
    - `q{ ++${%1}{a()} ...}`: `%1` is the reduced quantity, and `a()` is the
      first column of the line. (We wouldn't normally need parens, but if we
      omit them here Perl will assume `a` itself is the key.) We're
      incrementing the entry within the `%{%1}` hash.
    - `&& %1` is a way to return the hash reference as the reduced value (since
      we're modifying it in place). We need to do this without using a comma
      because each reducer function is evaluated in list context.

Of course, in this case it's a lot easier to use the streaming count operator:

```bash
$ ni /etc/passwd FWpsplit// r/[a-z]/gcx
a	39
b	36
c	14
d	13
e	17
f	1
g	11
h	20
i	46
k	3
l	19
m	14
n	50
o	25
p	15
r	24
s	51
t	15
u	17
v	12
w	12
x	23
y	12
```

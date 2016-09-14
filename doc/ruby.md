# Ruby interface
The `m` operator (for "map") lets you use a Ruby line processor to transform
data. The Ruby and [Perl](perl.md) drivers work very differently, in part to
reflect the cultural differences between the two languages.

```bash
$ ni n5m'ai * ai'               # square some numbers
1
4
9
16
25
```

Like `p`, `m` has lambda-end detection using Ruby syntax checking:

```bash
$ ni :rbfoo[n4m'ai*ai']
1
4
9
16
```

## Basic stuff
`a` to `q` are one-letter functions that return the first 17 tab-delimited
values from the current line. `r(...)` is a function that takes a list of
values and returns a tab-delimited row. You can suffix a value to coerce it:

- `f`: `to_f`
- `i`: `to_i`
- `s`: `to_s`

```bash
$ ni n4m'r a, ai + 1'                   # generate two columns
1	2
2	3
3	4
4	5
$ ni n4m'r a, ai + 1' m'r ai + bi'      # ... and sum them
3
5
7
9
```

Note that whitespace is required after every `m'code'` operator; otherwise ni
will assume that everything following your quoted code is also Ruby.

### `fields`: the array of fields
The Ruby code given to `m` is invoked on each line of input, which is stored in
`$l` and is the receiver. `$l` isn't split into fields until you call `fields`,
at which point the split happens and the fields are cached for efficiency.

```bash
$ ni /etc/passwd F::r3
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
bin	x	2	2	bin	/bin	/bin/sh
$ ni /etc/passwd F::r3m'r fields[0..3]'
root	x	0	0
daemon	x	1	1
bin	x	2	2
$ ni /etc/passwd F::r3m'r fields[1..3]'
x	0	0
x	1	1
x	2	2
$ ni /etc/passwd F::r3m'r fields.size'
7
7
7
```

### `r`, multiple rows, and return values
If your code returns an Enumerable, you'll get an output line for each entry.
If it returns a string, you'll have a single output line. `r()` is a function
that joins on tabs and returns the result (unlike the `r` function in the Perl
driver):

```bash
$ ni n2m'[a, ai + 100]'                 # multiple lines
1
101
2
102
$ ni n2m'r a, ai + 100'                 # multiple columns
1	101
2	102
```

ni will remove newlines from every string you give it. This makes it easier to
use backticks without any filtering.

## Buffered readahead
`m` code can read forwards in the input stream:

- `lines = rw {|l| condition}`: read lines while a condition is met
- `lines = ru {|l| condition}`: read lines until a condition is met
- `lines = re {|l| l.ai + l.bi}`: read lines while `ai + bi` is equal

```bash
$ ni n10m'r ru {|l| l.ai%4 == 0}'       # read forward until a multiple of 4
1	2	3
4	5	6	7
8	9	10
```

`ru`, etc return arrays of Line objects, and there are two ways to break them
into columns. Let's generate some columnar data:

```bash
$ ni n10m'r (1..10).map {|x| ai*x}' =\>mult-table
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
```

The first way is to access the fields on each line individually:

```bash
$ ni mult-table m'r ru {|l| l.ai%4 == 0}.map(&:g)'      # access column G
7	14	21
28	35	42	49
56	63	70
```

The other way is to invoke field accessor methods on the whole array:

```bash
$ ni mult-table m'r ru {|l| l.ai%4 == 0}.g'
7	14	21
28	35	42	49
56	63	70
```

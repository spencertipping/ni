# Using ni
ni is complex and subtle, and the learning curve is an overhang. Here's the
documentation.

## The basics
ni is `less` if you use it on files:

```bash
$ echo foo > bar
$ ni bar
foo
```

It's more useful than `less` in some ways, though, in that it knows how to
decode common compressed formats:

```bash
$ echo foo | gzip > bar.gz
$ ni bar.gz
foo
```

If you list multiple files, ni will concatenate them:

```bash
$ echo foo > bar
$ echo bif | gzip > baz
$ ni bar baz
foo
bif
```

Notice that ni knew to decompress the gzip file even though it didn't have a
`.gz` extension.

If you run ni on a directory, you'll get a list of its contents:

```bash
$ mkdir tmp
$ touch tmp/a tmp/b tmp/c
$ ni tmp
tmp/a
tmp/b
tmp/c
```

## Transforming data
ni parses its command line from left to right, treating each element as a
data-transform operator. Files append themselves and directories append their
contents, which is why the examples above worked.

### Selecting rows using `r`
Some operators transform the stream in-place, such as `r`. For example:

```bash
$ seq 1000 > data
$ ni data r4            # first four rows
1
2
3
4
$ ni data r-996         # all but the first 996 rows
997
998
999
1000
$ ni data r+4           # last four rows
997
998
999
1000
```

`r` can also sample data:

```bash
$ ni data rx250         # every 250th row
250
500
750
1000
$ ni data r.005         # randomly select 0.5% of all rows
1
272
544
816
$ ni data r/[2468]00$/  # select rows matching /[2468]00$/
200
400
600
800
```

When used to "random sample", the seed is consistent; this way ni doesn't
introduce nondeterminism into your process.

### Sorting
ni has four operators to sort things:

- `g`: Sort ascending with default options
- `G`: Sort ascending, but emit only unique rows
- `o`: Sort numerically ascending (`sort -n`)
- `O`: Sort numerically descending (`sort -rn`)

These all use the UNIX `sort` command, which overflows gracefully onto disk
and optionally compresses its temporary files. You can dictate which options
are passed to `sort` using `$NI_SORT_FLAGS` -- by default, ni will specify a
minimal and POSIX-compatible set of options.

```bash
$ ni data gr4                   # alpha-sort on numbers
1
10
100
1000
$ ni data data gr4              # data contains duplicates
1
1
10
10
$ ni data data Gr4              # duplicates removed by G
1
10
100
1000
$ ni data or4
1
2
3
4
$ ni data Or4
1000
999
998
997
```

Notice that operators can be joined without any whitespace. You can also write
them as separate elements (and sometimes you have to, as I'll explain later):

```bash
$ ni data O r4
1000
999
998
997
```

### Spreadsheet transformation
The first "serious" operator ni gives you is `m`, which maps a spreadsheet
across rows of input using a Ruby DSL. Rows are delimited by newlines and
columns by tabs. Let's construct a data file from `/usr/share/dict/words`
containing the first letter, the word, and the length of the word (I'll
explain this command as we go):

```bash
$ ni /usr/share/dict/words m'r a0s[0], as, as.size' > data
$ ni data r4
A	A	1
A	A's	3
A	AA's	4
A	AB's	4
```

When ni maps a spreadsheet over this data, it will look like this:

```
   |     A     |     B     |      C      | ...
---+-----------+-----------+-------------+----
 0 |A          |A          |1            |
 1 |A          |A's        |3            |
 2 |A          |AA's       |4            |
 3 |A          |AB's       |4            |
 ...
```

Although you can look downwards from where you are, ni is still mapping row by
row. This happens by changing the row numbers and dropping the top one out
after each iteration. As a result, you can refer to a single cell and get a
stream of data:

```bash
$ ni data m'b0' | wc -l         # streaming IO: O(1) memory usage
99171
$ ni data m'b0' r4              # same thing here
A
A's
AA's
AB's
```

None of this would be very useful if you had to stick to single-cell
expressions, of course, so you can use the `r` function to emit a row with
multiple values:

```bash
$ ni data m'r b0, c0' r4
A	1
A's	3
AA's	4
AB's	4
```

Now let's use downward references to measure the differences between word
lengths:

```bash
$ ni data m'r b0, c1 - c0' r4           # wrong; emits error cells
ni!undefined method `-' for "3":String
ni!undefined method `-' for "4":String
ni!undefined method `-' for "4":String
ni!undefined method `-' for "5":String
```

Error cells in ni have the pattern `ni!\001<message>`. They're one of two
binary magic prefixes ni has, the other being a valid packet (which begins
with `ni!\0`). In general you won't encounter these unless something is wrong
with your pipeline.

We can fix the problem above by calling `to_i`, or more concisely by appending
an `i` onto each cell reference:

```bash
$ ni data m'r b0, c1i - c0i' r4         # c1i and c0i coerce c1 and c0 to ints
A	2
A's	1
AA's	0
AB's	1
```

The full set of coercions:

```bash
$ ni data r1m'r ci.class'               # coerce to integer
Fixnum
$ ni data r1m'r cs.class'               # coerce to string
String
$ ni data r1m'r cf.class'               # coerce to float
Float
$ ni data r1m'r c.class'                # no coercion
String
```

The `s` coercion is useful for nonexistent values:

```bash
$ ni data r1m'r d.class'                # this cell is outside the data
NilClass
$ ni data r1m'r ds.class'               # coerce to string anyway
String
```

ni is still moving down by just one row at a time, because although we're
examining downward cells we aren't _consuming_ them. You can tell ni to
consume things by appending `!` to a cell. For example, let's go two at a
time:

```bash
$ ni data m'r b0, c1i! - c0i' r4        # c1i! = consume row containing c1
A	2
AA's	0
ABM's	-1
ACTH's	-2
```

The `0` suffix is implied if you leave it off, since the most common use case
is to operate on just one input row at a time:

```bash
$ ni data m'r b, c1i! - ci' r4          # b = b0, ci = c0i
A	2
AA's	0
ABM's	-1
ACTH's	-2
```

#### Ranges
Let's make a new dataset with a bunch of columns. I'm using a shell command
data source (with prefix `$:`) to generate numbers.

```bash
$ ni $:'seq 1000' m'r (1..10).map {|x| ai**x}' > data
$ ni data r4
1	1	1	1	1	1	1	1	1	1
2	4	8	16	32	64	128	256	512	1024
3	9	27	81	243	729	2187	6561	19683	59049
4	16	64	256	1024	4096	16384	65536	262144	1048576
```

We can select a column range using intervals, which are inclusive:

```bash
$ ni data r4m'r f_j'
1	1	1	1	1
64	128	256	512	1024
729	2187	6561	19683	59049
4096	16384	65536	262144	1048576
$ ni data r4m'r a_c'
1	1	1
2	4	8
3	9	27
4	16	64
```

Ranges evaluate to arrays, so all of the usual Ruby operators will work as you
expect:

```bash
$ ni data r4m'r a_c + e_f'
1	1	1	1	1
2	4	8	32	64
3	9	27	243	729
4	16	64	1024	4096
```

If you leave off the end of a range, ni will select all cells rightwards:

```bash
$ ni data r4m'r g_'
1	1	1	1
128	256	512	1024
2187	6561	19683	59049
16384	65536	262144	1048576
```

You can also construct row ranges:

```bash
$ ni data r4m'r a0_2 + b0_2'
1	2	3	1	4	9
2	3	4	4	9	16
3	4	9	16
4	16
```

Notice that since `r4` came before `m`, the spreadsheet ran out of downwards
lookahead as it reached the end of its input data. As a result it returned
shorter arrays than requested.

#### Multiple output rows
`r` emits a row each time you call it, which means you can turn one row into
many. For example, let's take each row of data and rotate it to be vertical:

```bash
$ ni data m'a_.each {|x| r x}' r4
1
1
1
1
```

If you return nil and don't call `r`, no row will be emitted. This can be
useful for filtering:

```bash
$ ni data m'r a_c if bi % 3 == 0' r4
3	9	27
6	36	216
9	81	729
12	144	1728
```

#### Indexing
You don't have to hard-code rows or columns. You can refer to any cell as a
pair of numbers using `_[row, col]`, and you can index any column by omitting
the row number. For example:

```bash
$ ni data r4m'r _[0, 0]'                # same as 'r a0'
1
2
3
4
$ ni data r4m'r a[0], b[0]'             # same as 'r a0, b0'
1	1
2	4
3	9
4	16
```

Indexing preserves structure, which works internally by calling `map` for any
non-numeric types. So you can do things like this:

```bash
$ ni data r4m'r a[0...4]'
1	2	3	4
2	3	4
3	4
4
```

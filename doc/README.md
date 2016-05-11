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
across rows of input. Rows are delimited by newlines and columns by tabs.
Let's construct a data file from `/usr/share/dict/words` containing the first
letter, the word, and the length of the word (I'll explain this command as we
go):

```bash
$ ni /usr/share/dict/words m'r as[0], as, as.size' > data
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

ni is still moving down by just one row at a time, because although we're
examining downward cells we aren't _consuming_ them. You can tell ni to
consume things by appending `!` to a cell. For example, let's go two at a
time:

```bash
$ ni data m'r b0, c1i! - c0i' r4        # c1i! = consume row containing c1
# NB: broken test
```

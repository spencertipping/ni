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

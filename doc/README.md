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

Some operators transform the stream in-place, such as `r` (which selects
rows). For example:

```bash
$ seq 1000 > data
$ ni data r4            # head -n 4
1
2
3
4
$ ni data r+4           # tail -n 4
997
998
999
1000
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

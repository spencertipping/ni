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
`.gz` extension; it looks at magic numbers.

If you run ni on a directory, you'll get a list of its contents:

```bash
$ mkdir tmp
$ touch tmp/a tmp/b tmp/c
$ ni tmp
tmp/a
tmp/b
tmp/c
```

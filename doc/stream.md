# Stream operations
Streams are made of text, and ni can do a few different things with them. The
simplest involve stuff that bash utilities already handle (though more
verbosely):

```bash
$ echo test > foo
$ ni foo
test
$ ni foo foo
test
test
```

ni transparently decompresses common formats, regardless of file extension:

```bash
$ echo test | gzip > fooz
$ ni fooz
test
$ cat fooz | ni
test
```

## Data sources
In addition to files, ni can generate data in a few ways:

```bash
$ ni $:'seq 4'                  # shell command stdout
1
2
3
4
$ ni n:4                        # integer generator
1
2
3
4
$ ni n0:4                       # integer generator, zero-based
0
1
2
3
$ ni id:foo                     # literal text
foo
```

## Transformation
ni can stream data through a shell process, which is often shorter than
shelling out separately:

```bash
$ ni n:3 | sort
1
2
3
$ ni n:3 $=sort                 # $= filters through a command
1
2
3
$ ni n:3 $='sort -r'
3
2
1
```

And, of course, ni has shorthands for doing all of the above:

```bash
$ ni n:3 g                      # g = sort
1
2
3
$ ni n:3g                       # no need for whitespace
1
2
3
$ ni n:3gAr                     # reverse-sort by first field
3
2
1
$ ni n:3O                       # more typical reverse numeric sort
3
2
1
```

See `ni //help/row` for details about row-reordering operators like sorting.

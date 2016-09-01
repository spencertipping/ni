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
$ ni n:3 g      # g = sort
1
2
3
$ ni n:3g       # no need for whitespace
1
2
3
$ ni n:3gAr     # reverse-sort by first field
3
2
1
$ ni n:3O       # NOTE: capital O, not zero; more typical reverse numeric sort
3
2
1
```

Notice that ni typically doesn't require whitespace between commands. The only
case where it does is when the parse would be ambiguous without it (and
figuring out when this happens requires some knowledge about how the shell
quotes things, since ni sees post-quoted arguments). ni will complain if it
can't parse something, though.

See [row.md](row.md) (`ni //help/row`) for details about row-reordering
operators like sorting.

## Writing files
You can write a file in two ways. One is, of course, using shell redirection:

```bash
$ ni n:3 >file
$ ni file
1
2
3
```

The other way is to copy the stream to a file:

```bash
$ ni n:3 \>file2
1
2
3
$ ni file2
1
2
3
```

If you want to write a compressed file, you can use the `Z` operator:

```bash
$ ni n:3Z >file3.gz
$ zcat file3.gz
1
2
3
```

`Z` lets you specify which compressor you want to use; for example:

```bash
$ ni id:gzip Z | gzip -dc               # gzip by default
gzip
$ ni id:gzip Zg | gzip -dc              # explicitly specify
gzip
$ ni id:gzip Zg9 | gzip -dc             # specify compression level
gzip
$ ni id:xz Zx | xz -dc
xz
$ ni id:lzo Zo | lzop -dc
lzo
$ ni id:lz4 Z4 | lz4 -dc
lz4
$ ni id:bzip2 Zb | bzip2 -dc
bzip2
```

## Checkpoints
Checkpoints let you cache intermediate outputs in a pipeline. This can avoid
expensive recomputation. For example, let's expensively get some numbers:

```bash
$ ni n:1000000gr4
1
10
100
1000
```

If we wanted to iterate on the pipeline from this point onwards, we could do
this quickly by checkpointing the result:

```bash
$ ni :numbers[n:1000000gr4]
1
10
100
1000
```

Now this data will be reused if we rerun it:

```bash
$ ni :numbers[n:1000000gr4]O
1000
100
10
1
```

ni isn't rerunning the process at all; we can see this by modifying the
checkpoint file:

```bash
$ echo 'checkpointed' > numbers
$ ni :numbers[n:1000000gr4]O
checkpointed
```

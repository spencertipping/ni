# Stream operations
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
$ ni n4                         # integer generator
1
2
3
4
$ ni n04                        # integer generator, zero-based
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
$ ni n3 | sort
1
2
3
$ ni n3 $=sort                  # $= filters through a command
1
2
3
$ ni n3 $='sort -r'
3
2
1
```

And, of course, ni has shorthands for doing all of the above:

```bash
$ ni n3 g       # g = sort
1
2
3
$ ni n3g        # no need for whitespace
1
2
3
$ ni n3gAr      # reverse-sort by first field
3
2
1
$ ni n3O        # NOTE: capital O, not zero; more typical reverse numeric sort
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
$ ni n3 >file                   # nothing goes to the terminal
$ ni file
1
2
3
```

The other way is to use one of ni's two file-writing operators:

```bash
$ ni n3 \>file2                 # writes the filename to the terminal
file2
$ ni file2
1
2
3
$ ni n3 \>%file3                # duplicates output
1
2
3
$ ni file3
1
2
3
```

The `<` operator inverts `>` by reading files; it's conceptually equivalent to
`xargs cat`:

```bash
$ ni n4 \>file3 \<
1
2
3
4
```

If you want to write a compressed file, you can use the `Z` operator:

```bash
$ ni n3Z >file3.gz
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
$ ni id:bzip2 Zb | bzip2 -dc
bzip2
```

```sh
# this one isn't a unit test because not all test docker images have a
# straightforward LZ4 install (some are too old)
$ ni id:lz4 Z4 | lz4 -dc
lz4
```

ni also provides a universal decompression operator `ZD`, though you'll rarely
need it because any external data will be decoded automatically. `ZD` has no
effect if the data isn't compressed.

```bash
$ ni n4 Z ZD
1
2
3
4
$ ni n4 ZD
1
2
3
4
```

Finally, ni provides the ultimate lossy compressor, `ZN`, which achieves 100%
compression by writing data to `/dev/null`:

```bash
$ ni n4 ZN | wc -c
0
```

## Checkpoints
Checkpoints let you cache intermediate outputs in a pipeline. This can avoid
expensive recomputation. For example, let's expensively get some numbers:

```bash
$ ni n1000000gr4
1
10
100
1000
```

If we wanted to iterate on the pipeline from this point onwards, we could do
this quickly by checkpointing the result:

```bash
$ ni :numbers[n1000000gr4]
1
10
100
1000
```

Now this data will be reused if we rerun it:

```bash
$ ni :numbers[n1000000gr4]O
1000
100
10
1
```

ni isn't rerunning the process at all; we can see this by modifying the
checkpoint file:

```bash
$ echo 'checkpointed' > numbers
$ ni :numbers[n1000000gr4]O
checkpointed
```

You can write compressed data into a checkpoint. The checkpointing operator
itself will decode any compressed data you feed into it; for example:

```bash
$ ni :biglist[n100000Z]r5
1
2
3
4
5
$ ni :biglist[n100000Z]r5
1
2
3
4
5
```

Checkpointing, like most operators that accept lambda expressions, can also be
written with the lambda implicit:

```bash
$ ni :biglist n100000Z r5
1
2
3
4
5
```

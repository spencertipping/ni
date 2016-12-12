# Stream operations
`bash` and `ni` are both pipeline constructors: they string processes together
by connecting one's stdin to another's stdout. For example, here's word count
implemented first in bash, then in ni (with the corresponding options
vertically aligned):

```
$ cat file |perl -lne 'print for split /\W+/' |sort |uniq -c
$ ni  file FW pF_                             g     c
```

ni's pipe symbols are implied: `ni g c` is the same as `ni g | ni c`.

```sh
$ ni --explain FW pF_ g c
["split_regex","(?^:[^\\w\\n]+)"]
["perl_mapper","F_"]
["row_sort","-t","\t"]
["count"]
```

## Files
Files append themselves to the data stream (the bash equivalent would be
`|cat - file`):

```bash
$ echo test > foo
$ ni foo
test
$ ni foo foo
test
test
```

ni automatically decompresses common formats (gz, lzo, lz4, xz, bzip2),
regardless of file extension:

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
$ ni +e'seq 4'                  # append output of shell command "seq 4"
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
$ ni n3 e'sort'                 # without +, e acts as a filter
1
2
3
$ ni n3e'sort -r'
3
2
1
$ ni n3e[ sort -r ]             # easy way to quote arguments
3
2
1
$ ni n3e[sort -r]
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
$ ni n3gA-      # reverse-sort by first field
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

### Important note about `e`
`e'sort -r'` and `e[sort -r]` are not quite identical; the difference comes in
when you use shell metacharacters:

```bash
$ mkdir test-dir
$ touch test-dir/{a,b,c}
$ ni e'ls test-dir/*'                   # e'' sends its command through sh -c
test-dir/a
test-dir/b
test-dir/c
$ ni e[ls test-dir/*] 2>/dev/null || :  # e[] uses exec() directly; no wildcard expansion
$ ni e[ ls test-dir/* ]                 # using whitespace avoids this problem
test-dir/a
test-dir/b
test-dir/c
```

## Stream combiners
ni has four operators that combine streams:

- `+`: append a stream to this one
- `^`: prepend a stream to this one
- `=`: duplicate this stream through a process, discarding its output

Visually, here's what these stream combiners do:

```
$ ni n10 n5 g           # stdin --> n10 --> n5 --> g --> stdout


                        #                  /dev/null --> n5 --> g
                        #                  ----------------------
                        #                           |
$ ni n10 +[n5 g]        # ni stdin --> n10 -------append--> ni stdout


                        #                        ni stdin --> n10
                        #                        ----------------
                        #                             |
$ ni n10 +[n5 g]        # /dev/null --> n5 --> g ---append---> ni stdout


                        #                  n5 --> g --> /dev/null
                        #                 /
$ ni n10 =[n5 g]        # ni stdin --> n10 -----------> ni stdout
```

```bash
$ { echo hello; echo world; } > hw
$ ni n3 +hw
1
2
3
hello
world
$ ni n3 ^hw
hello
world
1
2
3
$ ni hw =e[wc -l]               # output from 'wc -l' is gone
hello
world
```

## Writing files
You can write a file in two ways. One is, of course, using shell redirection:

```bash
$ ni n3 >file                   # nothing goes to the terminal
$ ni file
1
2
3
```

The other way is to use ni's `\>` operator:

```bash
$ ni n3 \>file2                 # writes the filename to the terminal
file2
$ ni file2
1
2
3
$ ni n3 =\>file3                # eats the filename because \> happens inside =
1
2
3
$ ni file3
1
2
3
```

The `\<` operator inverts `\>` by reading files; it's conceptually equivalent
to `xargs cat`:

```bash
$ ni n4 \>file3 \<
1
2
3
4
```

If you want to write a compressed file, you can use the `z` operator:

```bash
$ ni n3z >file3.gz
$ zcat file3.gz
1
2
3
```

`z` lets you specify which compressor you want to use; for example:

```bash
$ ni id:gzip z | gzip -dc               # gzip by default
gzip
$ ni id:gzip zg | gzip -dc              # explicitly specify
gzip
$ ni id:gzip zg9 | gzip -dc             # specify compression level
gzip
$ ni id:xz zx | xz -dc
xz
$ ni id:lzo zo | lzop -dc
lzo
$ ni id:bzip2 zb | bzip2 -dc
bzip2
```

```sh
# this one isn't a unit test because not all test docker images have a
# straightforward LZ4 install (some are too old)
$ ni id:lz4 z4 | lz4 -dc
lz4
```

ni also provides a universal decompression operator `zd`, though you'll rarely
need it because any external data will be decoded automatically. `zd` has no
effect if the data isn't compressed.

```bash
$ ni n4 z zd
1
2
3
4
$ ni n4 zd
1
2
3
4
```

Not to be outdone, ni provides the ultimate lossy compressor, `zn`, which
achieves 100% compression by writing data to `/dev/null`:

```bash
$ ni n4 zn | wc -c
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
$ ni :biglist[n100000z]r+5
99996
99997
99998
99999
100000
$ ni :biglist[n100000z]r+5
99996
99997
99998
99999
100000
```

Checkpointing, like most operators that accept lambda expressions, can also be
written with the lambda implicit. In this case the lambda ends when you break
the operators using whitespace:

```bash
$ ni :biglist n100000z r5
1
2
3
4
5
```

You can use `ni --explain` to see how ni parses something; in this case the
lambda is contained within the `checkpoint` array:

```bash
$ ni --explain :biglist n100000z r5
["checkpoint","biglist",[["n",1,100001],["sh","gzip"]]]
["head","-n",5]
$ ni --explain :biglist n100000zr5
["checkpoint","biglist",[["n",1,100001],["sh","gzip"],["head","-n",5]]]
```

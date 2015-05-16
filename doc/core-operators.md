## Core operators
ni always operates in terms of tab-delimited lines of text, and typically
buffers a line at a time into memory. Lines are automatically split into
fields, one per TSV column.

### Grouping and aggregation
Typically you use `-g` and `-a` together unless your data is pre-grouped. For
example, a local map/reduce workflow to count words looks like this:

```sh
$ ni README.md -r 'l.split(/\W+/).map {|word| row(word, 1)}' \
               -ga [ -1r% '%0.lazy.map(&:to_i).reduce(0, &:+)' ]

# equivalent, but much faster and shorter:
$ ni README.md -F \\W+ -1x1ga ^1s
```

### Multiple output lines
Any line-emitting operator can emit multiple output rows for a given input; to
do this, you return an array:

```sh
$ ni n:1000 -r '(0..5).map {row %0, %0 + 1}'
$ ni n:1000 -p 'map row(%0, %0 + 1), 1..5'
$ ni n:1000 -P '(row(%0, %0 + 1) for x in range(5))'
```

If your code returns `undef`, `None`, `nil`, or an empty array, no lines will
be emitted.

### Builtin functions
Sometimes what you're doing is so trivial that piping into an interpreter is
overkill. For example, maybe you're just summing a column. In that case you can
use one of ni's builtin functions:

```sh
$ ni -1r% '%0.lazy.map(&:to_i).reduce(0, &:+)'          # ruby
$ ni -1x% '0 ^+ lr'                                     # canard (builtin)
$ ni -1st+1                                             # stream ops
```

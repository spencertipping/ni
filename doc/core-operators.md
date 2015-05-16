## Core operators
ni always operates in terms of tab-delimited lines of text, and typically
buffers a line at a time into memory. Lines are automatically split into
fields, one per TSV column. These fields are stored in an array available
within any code you run:

- `[-m|--map] L'code'`: map each line through `code` in language L
- `[-k|--keep] L'code'`: keep lines for which `code` evaluates truthily
- `[-r|--reduce] L'code'`: reduce all lines through `code` in language L
- `[-a|--aggregate] L'code'`: aggregate key-wise and stream lines into `code`
  in language L
- `[-g|--group]`: Reorder lines so the first column is grouped
- `[-o|--order]`: Reorder lines so the first column is sorted

### Grouping and aggregation
Typically you use `-g` and `-a` together unless your data is pre-grouped. For
example, a local map/reduce workflow to count words looks like this:

```sh
$ ni README.md -mr'l.split(/\W+/).map {|word| row(word, 1)}' \
               -gar'row k, fs.lazy.map {|x| x[1].to_i}.reduce(0, &:+)'

# equivalent and shorter:
$ ni README.md -F\\W+ -1m/1 -ga ^1s
```

### Supported languages
In the code above, the lower-case `r` prefix on the code specifies that the
code is written in Ruby. Here's the list of languages ni knows about:

- `/`: [Canard](doc/canard.md)
- `f`: line filter shell process
- `r`: Ruby
    - line is stored as `l`
    - fields are in an array called `f`
    - reduced fields are in `r`
- `p`: Python; variables are the same as Ruby
- `P`: Perl; line is stored as `$l`, fields are in `@f`, reduced fields in `@r`
- `j`: Java
    - line is `String l` (no newline)
    - fields are `String[] f`
    - long-parseable fields are `long[] fi` (`Long.MIN_VALUE` otherwise)
    - double-parseable fields are `double[] fd` (`NaN` otherwise)
- `c`: C99
    - line is `const char *l` (null-terminated, no newline), length is `n`
    - fields are null-terminated `const char *const *f` ending with `NULL`
    - field lengths are `const int *fl` ending with `-1`
    - number of fields is `int fn`
    - `row()` takes a type template string as its first argument
- `C`: Clojure
    - line is `String l` (no newline)
    - fields are a `PersistentVector` of `Object` called `f`
    - reduced fields are in a `PersistentVector` called `r`
- `S`: Scala
    - line is `String l` (no newline)

ni additionally defines the shorthands `%0`, `%1`, ..., `%9` to refer to
`f[0]`, `f[1]`, ..., `f[9]` in a language-specific way. Within reduce code,
`^0`, `^1`, etc refer to `r[]` values.

Each of these languages defines a function called `row`, which you should use
to efficiently emit lines to output:

```sh
$ seq 1000 | ni -mr'row(%0, %0 * %0)'
```

### Multiple output lines
Any line-emitting operator can emit multiple output rows for a given input; to
do this, you return an array:

```sh
$ seq 1000 | ni -mr'(0..5).map {row %0, %0 + 1}'
$ seq 1000 | ni -mp'(row(%0, %0 + 1) for x in range(5))'
$ seq 1000 | ni -mP'map row(%0, %0 + 1), 1..5'
```

If your code returns `undef`, `None`, `nil`, or an empty array, no lines will
be emitted.

### Builtin functions
Sometimes what you're doing is so trivial that piping into an interpreter is
overkill. For example, maybe you're just summing a column. In that case you can
use one of ni's builtin functions:

```sh
$ ni -1Rr'^0.to_i + %0.to_i'    # use ruby to sum stuff
$ ni -1R/+                      # use ni builtin + (same output, but faster)
```

A full list of builtin functions is provided later on, or you can see one by
running `ni --help-builtins`.

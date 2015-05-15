# ni: say "ni" to data
![ni!](http://spencertipping.com/ni.png)

MIT license as usual.

# Introduction
ni is a tool that understands how to move and transform most common types of
structured data. ni is self-compiling, requires no installation or
dependencies, and should run on any POSIX.2001-compliant system including
Linux, OSX, and most other Unices.

## Dependencies
ni has no core dependencies beyond libc and a C compiler, but certain features
won't work unless command-line tools exist on your `$PATH`:

- PostgreSQL IO requires the `psql` executable
- SQLite IO requires the `sqlite3` executable
- Lucene and other Java integration requires the `mvn` executable and a working
  Internet connection; it assumes Java v5 or later and Maven 2 or later
- S3 access requires `s3cmd`
- HTTP GET/PUT requires `curl`
- Paging requires either `less` or `more`, defaulting to `less`
- Remoting requires `ssh`
- Transparent decompression requires relevant executables (`gz`, `lzop`,
  `bzip2`, `xz`)

In addition, programming-language integrations require the relevant
executables:

- Ruby requires `ruby` version 1.8.7 or greater
- Python requires `python`, version 2.4 or greater
- Perl requires `perl`, version 5.0 or greater
- R requires `R`, version 3 or greater
- C requires `c99`
- Java requires `javac`, version 5 or greater
- Clojure requires `lein`, version 2 or greater
- Scala requires `scala`, version 2 or greater

ni generates wrapper code for each of these environments, and does so in a way
that maximizes version/library compatibility. This means, for example, that you
can use Ruby 2 and get lazy iterators; but 1.8.7 will also work with standard
`Enumerator` instances. ni's adapter code will work with either version.

## Motivation
Your shell is the REPL of your system; it can process streams of arbitrary size
in rudimentary ways. It provides no abstraction capabilities, however; it's a
delegation language, not a computing language.

ni bridges that gap. It knows how to talk to SQL databases, run Hadoop jobs,
build and query Lucene indexes, speak HTTP/XML/HTML/JSON, plot things in R and
gnuplot, create and build one-off Maven projects, compile C programs, open
sockets, distribute work with SSH, and run one-liners in various programming
languages. It also knows how write custom binary protocols to connect the
pieces together in efficient ways, and it implements adaptive statistical
parsers for XML and JSON (which should converge to near-optimal average case
performance for your data).

In other words, ni is designed to be the easiest and fastest way to process
data.

# Documentation by example
## Survival commands
![!](http://spencertipping.com/ni2.png)

ni is terse and shares many properties with write-only languages. Because of
this you'll probably want to make use of its documentation/previewing
operators, which always go at the end:

```sh
$ ni -n1r+10m/ --explain
        --cat fd:0
-n      --number
-1r+    --address 1 --reduce +
-10m/   --address 10 --map /

$ { echo '5'; echo '6'; } | ni -n1r+10m/ --trace
0               5
1       -n      1       5
2       -1r+    1       5
3       -10m/   5
0               6
1       -n      2       6
2       -1r+    2       11
3       -10m/   5.5
```

You can also ask ni for a list of supported operators, quasi-file syntaxes, and
configuration variables:

```sh
$ ni --help
$ ni --help-builtins
$ ni --help-operators
$ ni --help-quasifiles
$ ni --help-env
```

## Simple stream manipulation
ni behaves like a smarter version of `less` if you specify no transformation
operators:

```sh
$ ni                            # pipe from stdin to stdout
$ ni file1 file2                # previews contents with 'less'
$ ni file1.gz file2.bz2         # automatically decompresses stuff
$ ni http://google.com          # download HTTP
$ ni me@host:file.gz            # transfers over SSH
$ ni hdfs:/path/to/data.gz      # retrieves over HDFS
$ ni s3://foo/bar/bif           # retrieves with s3cmd
$ ni sql3:/tmp/mydb.db:foo      # convert SQLite 'foo' table to TSV
$ ni sql3::'select * from foo'  # query results from ${TMPDIR:-/tmp}/$USER.db
$ ni psql::mydb:foo             # ditto, but PostgreSQL
$ ni lucene:/path               # all documents from lucene index
$ ni lucene:/path:'{"foo":"bar"}
$ ni fd:3                       # stream data from file descriptor 3
$ ni perl:'0..99'               # stream from perl
$ ni ruby:'(0...100).map {|x| x + 1}'
$ ni python:'range(50)'
$ ni sh:'find . -name foo'      # stream from shell command stdout
$ ni n:100                      # numbers from 0 to 99 inclusive
```

All ni commands start with `-` for short options or `--` for long ones:

```sh
$ ni n:100 --pipe shuf -T1      # choose random number in [0, 99]
$ ni n:1000 -m r'row _0 * _0'   # use Ruby to square each number
$ ni n:1000 -m00*               # builtin multiplication operator
```

## Basic stream operators
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
$ ni README.md -m r'l.split(/\W+/).map {|word| row(word, 1)}' \
               -g1a r'row ls.lazy.reduce(0, &:+)'
```

### Supported languages
In the code above, the lower-case `r` prefix on the code specifies that the
code is written in Ruby. Here's the list of languages ni knows about:

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
$ seq 1000 | ni -m r'row(%0, %0 * %0)'
```

### Multiple output lines
Any line-emitting operator can emit multiple output rows for a given input; to
do this, you return an array:

```sh
$ seq 1000 | ni -m r'(0..5).map {row %0, %0 + 1}'
$ seq 1000 | ni -m p'(row(%0, %0 + 1) for x in range(5))'
$ seq 1000 | ni -m P'map row(%0, %0 + 1), 1..5'
```

If your code returns `undef`, `None`, `nil`, or an empty array, no lines will
be emitted.

### Builtin functions
Sometimes what you're doing is so trivial that piping into an interpreter is
overkill. For example, maybe you're just summing a column. In that case you can
use one of ni's builtin functions:

```sh
$ ni -1r r'^0 + %0'             # use ruby to sum stuff
$ ni -1r+                       # use ni builtin + (same output, but faster)
```

A full list of builtin functions is provided later on, or you can see one by
running `ni --help-builtins`.

## Addressing
Every operator has an implicit "address", or set of columns upon which it
operates. The default address of every builtin operator is either all columns
(for operators that make sense against multiple fields, like `-m`), or just the
first column (for single-value operators like `-g`). You can readdress an
operator by prefixing it with an address spec, which takes one of these two
forms:

- `\d+`, e.g. `-041m r'foo'`: address just the columns at the specified base-10
  indexes
- `[0-9a-zA-Z]+@`, e.g. `-bc@m r'foo'`: indexes in base-62 (0-9, a-z, A-Z)

Addresses can duplicate and reorder columns. Unaddressed columns pass through
unmodified. For example:

```sh
$ ni /usr/share/dict/words -m r'row %0, l.size' > wsizes
$ ni wsizes -1m r'%0 * %0'              # word, size²
$ ni wsizes -11m r'%0 + %1'             # word, size + size
```

Operators like `-a` can collapse multiple lines into fewer outputs. In this
case the unaddressed fields are treated as columns and truncated to fit the
number of output rows. If there are more outputs than inputs, extra output rows
will have blank values.

If an operator emits fewer columns than its address, the unspecified columns
are filled with blank values. If it emits more, extra columns are tacked onto
the end of the input row (after assembling all addressed column values).

## Builtin functions
**This all might change**

Builtin functions are concatenative and manipulate an operand stack initially
comprised of the addressed fields of input.

- `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `!`, `<`, `>`, `=`
- `_sin`, `_cos`, `_tan`, `_log`, `_exp`, `_log2`, `_exp2`, `_sqr`, `_sqrt`
- `_r`, `_w`, `_a`: read/write/append quasifile
- `_rp`, `_pr`: rect to/from polar
- `_ghe`, `_ghd`: geohash encode/decode
- `_be`, `_bd`: binary encode/decode (like Perl/Ruby `pack`)
- `_q`: quantize
- `,`: juxtapose output of other functions (commutative, associative)

Composition works like this:

```sh
$ ni n:10 -000m *+      # calculate (x * x) + x for x in 0..9
$ ni n:10 -00m *,+      # calculate x*x and x+x for x in 0..9
```

## Lambdas
Because ni's command-line arguments are concatenative, you can quote a list
and have that represent a stream transformer. For example:

```sh
$ ni n:5 -00m*
$ ni n:5 [ -00m* ] --eval               # same as above
$ ni n:5 [ [ -00m* ] --eval ] --eval    # ditto
```

Some commands like `-a` and `-r` process streams rather than individual rows,
so you can pass a lambda rather than a piece of code:

```sh
$ ni n:1000 -r+                 # binary fold
$ ni n:1000 -r [ -r+ ]          # fold-all, streaming into a binary fold
```

## Encoded data
ni knows about several common text formats:

- JSON
- XML/HTML
- [Geohash](https://en.wikipedia.org/wiki/Geohash)
- [WKT](https://en.wikipedia.org/wiki/Well-known_text)

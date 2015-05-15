# ni: say "ni" to data
![ni!](http://spencertipping.com/ni.png)

![!](http://spencertipping.com/ni2.png)

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

- Ruby requires `ruby` version 1.9 or greater
- Python requires `python`, version 2.4 or greater
- Perl requires `perl`, version 5.0 or greater
- R requires `R`, version 3 or greater

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
ni is terse and shares many properties with write-only languages. Because of
this you'll probably want to make use of its documentation/previewing
operators, which always go at the end:

```sh
$ ni -n1r+10m/ --explain
	fd:0
-n	--number
-1r+	--address 1 --reduce +
-10m/	--address 10 --map /

$ { echo '5'; echo '6'; } | ni -n1r+10m/ --trace
0		5
1	-n	1	5
2	-1r+	1	5
3	-10m/	5
0		6
1	-n	2	6
2	-1r+	2	11
3	-10m/	5.5
```

You can also ask ni for a list of supported operators, quasi-file syntaxes, and
configuration variables:

```sh
$ ni --help
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
$ ni n:1000 -@m '_0 * _0'       # use Ruby to square each number
```

## Basic stream operators
ni always operates in terms of tab-delimited lines of text, and typically
buffers a line at a time into memory. Lines are automatically split into
fields, one per TSV column. These fields are stored in an array available
within any code you run:

- `[-m|--map] L'code'`: map each line through `code` in language L
- `[-k|--keep] L'code'`: keep lines for which `code` evaluates truthily
- `[-r|--reduce] L'code'`: reduce all lines through `code` in language L
- `[-a|--aggregate] [ni args]`: aggregate key-wise and stream lines into
  `[ni args]`
- `[-g|--group]`: Reorder lines so the first column is grouped
- `[-o|--order]`: Reorder lines so the first column is sorted

Typically you use `-g` and `-a` together unless your data is pre-grouped. For
example, a local map/reduce workflow to count words looks like this:

```sh
$ ni README.md -m r'l.split(/\W+/).map {|word| row(word, 1)}' \
               -ga [ -1s ]
```

In the code above, the lower-case `r` prefix on the code specifies that the
code is written in Ruby. Here's the list of languages ni knows about:

- `r`: Ruby; the line is stored as `l`, fields are in an array called `f`
- `p`: Python; variables are the same as Ruby
- `P`: Perl; line is stored as `$l`, fields are in `@f`

ni additionally defines the shorthands `%0`, `%1`, ..., `%9` to refer to
`f[0]`, `f[1]`, ..., `f[9]` in a language-specific way.

Each of these languages defines a function called `row`, which you should use
to efficiently emit lines to output:

```sh
$ seq 1000 | ni -m r'row(%0, %0 * %0)'
```

Any line-wise operator can emit multiple output rows for a given input; to do
this, you return an array:

```sh
$ seq 1000 | ni -m r'(0..5).map {row %0, %0 + 1}'
$ seq 1000 | ni -m p'(row(%0, %0 + 1) for x in range(5))'
$ seq 1000 | ni -m P'map row(%0, %0 + 1), 1..5'
```

If your code returns `undef`, `None`, `nil`, or an empty array, no lines will
be emitted.

## Addressing
Every operator has an implicit "address", or set of columns. The default
address of every builtin operator is either all columns (for operators that
make sense against multiple fields, like `-m`), or just the first column (for
single-value operators like `-g`). You can readdress an operator by prefixing
it with an address spec, which takes one of these two forms:

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

## JSON
Get the value of a JSON field:

```sh
$ echo '{"foo":{"bar":[1,2,3]}}' > json
$ ni json -j .foo.bar[1]                # compound reference
$ ni json -j .foo -j .bar -j [1]        # split reference
```

Extract an array of JSON objects:

```sh
$ echo '[{"foo":1},{"bar":2}]' > json
$ ni json -j []                         # expand array into columns
```

## SQL
SQL destinations are available for both input and output, and ni infers schemas
as necessary:

```sh
$ seq 100 | ni --into sql3:mydb.db:foo_table            # infer schema
$ seq 100 | ni --into sql3:mydb.db:foo_table:'f0 int'   # specify schema
$ ni sql3:mydb.db:foo_table -0s                         # pull data
```

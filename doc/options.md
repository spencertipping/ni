# Complete ni operator listing
Implementation status:
- T: implemented and automatically tested
- M: implemented and manually tested (so it might be broken)
- P: partially implemented
- I: implemented
- U: unimplemented

## Resources
Scheme     | Status | Example
-----------|--------|--------
`file://`  | M      | `file:///usr/share/dict/words`
`hdfs://`  | M      | `hdfs:///user/x/foobar`
`http://`  | T      | `http://localhost:8090`
`https://` | M      | `https://en.wikipedia.org`
`s3cmd://` | I      | `s3cmd://some/path/to/object`

## Stream operators
Operator | Status | Example      | Description
---------|--------|--------------|------------
`+`      | T      | `+p'foo'`    | Appends a data source evaluated with no input
`^`      | T      | `^file`      | Prepends a data source
`=`      | T      | `=\>f`       | Duplicate stream, ignoring fork output
`\>`     | T      | `\>file`     | Sinks stream into resource, emits resource name
`\>\'R`  | M      | `\>\'R`      | Converts a stream of resource names into a packed resource stream
`\<`     | T      | `\<`         | Opposite of `\>`
`%`      | I      | `%n100`      | Interleave lines, optionally with a bias ratio
`-`      |        |              |
`_`      |        |              |
`\!`     | I      | `\!p'F_==3'` | Assert a condition
`,`      | T      | `,jAB`       | Enter cell context
`:`      | T      | `:foo[nE8z]` | Checkpointed stream
`::`     | M      | `::x[n100]`  | Create an in-memory data closure
`//:`    | M      | `//:x`       | Append closure data
`@`      | U      | `@foo[\>@a]` | Enter named-gensym context
`\##`    | U      | `\>foo \##`  | Cat **and then obliterate** named resource(s)
`1`      | M      | `1p'"hi"'`   | `1` is an alias for `n1`
`a`      |        |              |
`b`      | T      | `bL40`       | Binary operators
`c`      | T      | `c`          | `uniq -c`, but emits proper TSV format
`d`      |        |              |
`e`      | T      | `e[tac]`     | Exec shell command
`f`      | T      | `fACB`       | Reorder, duplicate, or drop fields by column
`g`      | T      | `gA`         | Sort by all or selected columns
`h`      |        |              |
`i`      | T      | `ifoo`       | Append literal text `foo`
`j`      | U      | `j foo`      | Join sorted streams on field values
`k`      |        |              |
`l`      | T      | `l'(1+ a)'`  | Map over rows using Common Lisp
`m`      | T      | `m'a + 1'`   | Map over rows using Ruby
`n`      | T      | `n10`        | Generate or prepend line numbers
`o`      | T      | `oC`         | Numeric sort ascending
`p`      | T      | `p'a + 1'`   | Map over rows using Perl
`q`      |        |              |
`r`      | T      | `r10`        | Select rows by criterion
`s`      | M      | `sfoo[n10]`  | Evaluate a lambda on another machine using SSH
`t`      |        |              |
`u`      | T      | `u`          | Just like `uniq`
`v`      | T      | `vCplc`      | Vertically transform a range of columns
`w`      | T      | `wn100`      | "With": join a stream rightwards
`x`      | T      | `xC`         | Exchange first fields with others
`y`      |        |              |
`z`      | T      | `z4`         | Compress or decompress
`A`      |        |              |
`B`      | T      | `Bn`         | Buffer a stream
`C`      | T      | `Cubuntu[g]` | Containerize a pipeline with Docker
`D`      | PT     | `D:foo`      | Destructure structured text data (JSON/XML)
`E`      | T      | `Efoo[g]`    | Execute a pipeline in an existing Docker
`F`      | T      | `FC`         | Parse data into fields
`G`      | T      | `GA...`      | Gnuplot prefix
`H`      | T      | `HS:::`      | Send named files through hadoop
`I`      | I      | `I[...]`     | Process concatenated image files
`J`      |        |              |
`K`      |        |              |
`L`      | U      | `L'(1+ a)'`  | (Reserved for Lisp driver)
`M`      | U      | `M'svd(x)'`  | Faceted Octave matrix interop
`MM`     | I      | `MM`         | [Map-o-matic](https://github.com/spencertipping/mapomatic)
`N`      | T      | `N'svd(x)'`  | Faceted NumPy matrix interop
`O`      | T      | `OD`         | Numeric sort descending
`P`      | T      | `PLg`        | Evaluate Pyspark lambda context
`Q`      |        |              |
`R`      | U      | `R'a+1'`     | Faceted R matrix interop
`S`      | T      | `S16[rx100]` | Scale across multiple processes
`T`      |        |              |
`U`      |        |              |
`V`      | U      | `VB`         | Pivot and collect on field B
`W`      | T      | `Wn100`      | "With": join a stream leftwards
`X`      | T      | `X`          | Sparse to dense matrix conversion
`Y`      | T      | `Y`          | Dense to sparse matrix conversion
`Z`      | T      | `Z2`         | Fold/unfold stream to specified width


## Cell operators
Operator | Status | Example | Description
---------|--------|---------|------------
`h`      | T      | `,z`    | Turns each unique value into a hash.
`H`      | T      | `,HAB`  | Turns each unique value into a unique number between 0 and 1.
`z`      | T      | `,h`    | Turns each unique value into an integer.

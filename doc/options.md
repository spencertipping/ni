# Complete ni operator listing
Operator | Example      | Description
---------|--------------|------------
`+`      | `+p'foo'`    | Appends a data source evaluated with no input
`^`      | `^file`      | Prepends a data source
`%`      | `%[\>f K]`   | Duplicate stream, interleaving fork output
`=`      | `=\>f`       | Duplicate stream, ignoring fork output
`\>`     | `\>file`     | Sinks stream into resource, emits resource name
`\<`     | `\<`         | Opposite of `\>`
`.`      | `.n100`      | Interleave lines, optionally with a bias ratio
`-`      |              |
`_`      |              |
`,`      | `,jAB`       | Enter cell context
`:`      | `:foo[nE8z]` | Checkpointed stream
`@`      | `@foo[\>@a]` | Enter named-gensym context
`\##`    | `\>foo \##`  | Cat **and then obliterate** named resource(s)
         |              |
`a`      |              |
`b`      | `bL40`       | Block-read and unpack binary data
`c`      | `c`          | `uniq -c`, but emits proper TSV format
`d`      |              |
`e`      | `e[tac]`     | Exec shell command
`f`      | `fACB`       | Reorder, duplicate, or drop fields by column
`g`      | `gA`         | Sort by all or selected columns
`h`      |              |
`i`      |              |
`j`      | `j foo`      | Join sorted streams on field values
`k`      |              |
`l`      | `l'(1+ a)'`  | Map over rows using Common Lisp
`m`      | `m'a + 1'`   | Map over rows using Ruby
`n`      | `n10`        | Generate or prepend line numbers
`o`      | `oC`         | Numeric sort ascending
`p`      | `p'a + 1'`   | Map over rows using Perl
`q`      |              |
`r`      | `r10`        | Select rows by criterion
`s`      | `sfoo[n10]`  | Evaluate a lambda on another machine using SSH
`t`      |              |
`u`      | `u`          | Just like `uniq`
`v`      | `vCplc`      | Vertically transform a range of columns
`w`      | `wn100`      | "With": horizontally juxtapose two streams
`x`      | `xC`         | Exchange first fields with others
`y`      |              |
`z`      | `z4`         | Compress or decompress
         |              |
`A`      |              |
`B`      |              |
`C`      | `Cubuntu[g]` | Containerize a pipeline with Docker
`D`      | `D.foo`      | Destructure structured text data (JSON/XML)
`E`      |              |
`F`      | `FC`         | Parse data into fields
`G`      |              |
`H`      | `H c`        | Send named files through hadoop
`I`      |              |
`J`      |              |
`K`      |              |
`L`      | `L'(1+ a)'`  | (Reserved for Lisp driver)
`M`      | `M'svd(x)'`  | Faceted Octave matrix interop
`N`      | `N'svd(x)'`  | Faceted NumPy matrix interop
`O`      | `OD`         | Numeric sort descending
`P`      | `Plg`        | Evaluate Pyspark lambda context
`Q`      |              |
`R`      | `R'a+1'`     | Faceted R matrix interop
`S`      |              |
`T`      |              |
`U`      |              |
`V`      | `VB`         | Pivot and collect on field B
`W`      |              |
`X`      | `X`          | Sparse to dense matrix conversion
`Y`      | `Y`          | Dense to sparse matrix conversion
`Z`      |              |

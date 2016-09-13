# Complete ni operator listing
## General patterns in streaming contexts
### Stream manipulation
- `+`: append a data source
- `^`: prepend a data source
- `%`: duplicate stream through a process (like `tee`)
- `\>`: convert stream to named resource
- `\<`: convert named resource(s) to stream

### Row/column/cell operations
- `d`: destructure objects of some kind (JSON, XML, etc)
- `f`: select/rearrange columns
- `m'code'`: map using quoted code in whatever language is appropriate
- `r`: select rows by specified criterion
- `x`: exchange values (e.g. key/value, columns, etc)

### Sorting/grouping/partitioning
- `g`: group by key or fields
- `c`: count grouped rows
- `u`: uniq
- `o`: total order
- `O`: reverse total order

## Root (toplevel) context
Operator | Example      | Description
---------|--------------|------------
`+`      | `+p'foo'`    | Appends a data source evaluated with no input
`^`      | `^file`      | Prepends a data source
`%`      | `%[\>f K]`   | Duplicate stream, interleaving fork output
`=`      | `=\>f`       | Duplicate stream, ignoring fork output
`\>`     | `\>file`     | Sinks stream into resource, emits resource name
`\<`     | `\<`         | Opposite of `\>`
`-`      |              |
`,`      | `,jAB`       | Enter cell context
`.`      |              |
`:`      |              |
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
`n`      | `n'svd(x)'`  | Map over data using NumPy
`o`      | `oC`         | Numeric sort ascending
`p`      | `p'a + 1'`   | Map over rows using Perl
`q`      |              |
`r`      | `r10`        | Select rows by criterion
`s`      | `sfoo[n10]`  | Evaluate a lambda on another machine using SSH
`t`      |              |
`u`      | `u`          | Just like `uniq`
`v`      | `vCplc`      | Vertically transform a range of columns
`w`      |              |
`x`      | `xC`         | Exchange first fields with others
`y`      |              |
`z`      | `z4`         | Compress or decompress
         |              |
`A`      |              |
`B`      |              |
`C`      |              |
`D`      | `D.foo`      | Destructure structured text data (JSON/XML)
`E`      |              |
`F`      | `FC`         | Parse data into fields
`G`      |              |
`H`      | `H c`        | Send named files through hadoop
`I`      |              |
`J`      |              |
`K`      | `K`          | Kills all data in the stream
`L`      |              |
`M`      |              |
`N`      |              |
`O`      | `OD`         | Numeric sort descending
`P`      | `Plg`        | Evaluate Pyspark lambda context
`Q`      |              |
`R`      | `R'a+1'`     | R interop
`S`      |              |
`T`      |              |
`U`      |              |
`V`      | `VB`         | Pivot and collect on field B
`W`      |              |
`X`      |              |
`Y`      |              |
`Z`      |              |

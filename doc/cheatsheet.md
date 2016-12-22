#Cheatsheet

`ni` is cheating already, so consider this a meta-cheatsheet of `ni` operations.


`$ni ...`

##Basic I/O Operations
* `n`: Integer stream
  * `n#`: Stream the sequence `1..#`, one number per line
  * `n0#`: Stream the sequence `0..#-1`, one number per line
  * `n`: Stream the sequence `1..`, one number per line.
* `filename{, .gz, .bz, .xz, .lzo, .txt, .csv, etc}`: File input
  * Automatically decompresses the file and streams out one line of the file out at a time.
* `input_source \<`: Read from `input_source`
  * This 
* `z`: Compress stream
  * Defaults to applying `gzip` compression, but can use any number of operations. 
  * `zg`: explicit gzip with default compression
  * `zg9`: gzip with compression level 9
  * `zn` writes all output to `dev/null`; useful for forcing an operation to complete.
  * `zo`: lzo compression
  * `zx`: xzip compression
  * `zb`: bzip2 compression
  * `z4`: lz4 compression (note, some Docker images have a too-old version of LZ4)
* ` > filename`: Redirect to file
  * Redirects the stream to `filename`, emits nothing
  * This is not a `ni` operation, just a file redirect.
* ` \>filename`: Write and emit filename:
  * Writes the stream to the file named `filename`, and emits the filename.
  * Note that there is no whitespace between the angle-bracket and the filename.
* `e[<script>]`: Evaluate script
  * evaluate `<script>` in bash, and stream out the results one line at a time.
* `id:<text>`: Literal text input
  * `$ ni id:OK!` -- add the literal text `OK!` to the stream.
  * `$ ni id:'a cat'` -- add the literal text `a cat` to the stream. The quotes are necessary to instruct the `ni` parser where the boundaries of the string are. Double quotes will work as well.

##Basic Row Operations
* `r`: Take rows
  * `$ ni <data> r3` - take the first 3 rows of the stream
  * `$ ni <data> r~3` - take the last 3 rows of the stream
  * `$ ni <data> r100x` - take every 100th row in the stream
  * `$ ni <data> r.05` - sample 5% of the rows in the stream.
    * The sampling here is deterministic (conditioned on the environment variable `NI_SEED`) and will always return the same set of rows.
  * `$ ni <data> r/<regex>/` - take rows where `<regex>` matches.
*  `p'<...>'`: Perl
   * applies the Perl snippet `<...>` to each row of the stream 
   * See the [Perl for ni](perl_for_ni.md) docs
*  `m'<...>'`: Ruby
   * applies the Ruby snippet `<...>` to each row of the stream 
*  `l'<...>'`: Lisp
   * applies the Lisp snippet `<...>` to each row of the stream 

##Basic Column Operations
Columns are referenced "Excel-style"--the first column is `A`, the second is `B`, etc.

* `f`: Take columns
  * `$ ni <data> fAA` - select the first column and duplicate it
  * `$ ni <data> fAD.` - select the first column and all remaining columns starting with the fourth
  * `$ ni <data> fB-E` - select columns 2-5
  * `$ ni <data> fCAHDF` - selects columns 3, 1, 8, 4, 6, and streams them out in that order.
* `F`: Split stream into columns
  * `F:<char>`: split on character
  * `F/regex/`: split on occurrences of regex. If present, the first capture
  group will be included before a tab is appended to a field.
  * `Fm/regex/`: don't split; instead, look for matches of regex and use those as
  the field values.
  * `FC`: split on commas (doesn't handle special CSV cases)
  * `FV`: parse CSV "correctly", up to newlines in fields
  * `FS`: split on runs of horizontal whitespace
  * `FW`: split on runs of non-word characters
  * `FP`: split on pipe symbols
* `x`: Exchange columns
  * `x` -- exchange the first two columns. 
    * Equivalent to `fBA.`
  * `xC` -- exchange column 3 with column 1. 
    * Equivalent to `fCBA.`
  * `xBE` -- exchange columns 2 and 5 with columns 1 and 2. 
    * This runs in order, so `B` will be switched with `A` first, which will then be switched with column `E`. 
    * Equivalent to `fBECDA.`

  
##Sort, Unique & Count Operations

Sorting is often a rate-limiting step in `ni` jobs run on a single machine, and data will need to be buffered to disk if a sort is too large to fit in memory. If your data is larger than a gigabyte uncompressed, you may want to take advantage of massively distributing the workload through Hadoop operations.

* `g`: General sorting
  * `gB` - sort rows ascending by the lexicographic value of the second column
    * Lexicographic value is determined by the ordering of characters in the ASCII table.
    * `ni id:a id:C g` will put the capital `C` before the lower-case `a`, because capital Latin letters precede lowercase Latin letters in ASCII.
  * `gC-` - sort rows *descending* by the lexicographic value of the third column
   * `gCA-` - sort rows first by the lexicographic value of the third column, ascending. For rows with the same value for the third column, sort by *descending* value of the first column.
  * `gDn` - sort rows ascending by the *numerical* value of the fourth column.
    * The numeric sort works on integers and floating-point numbers written as decimals.
    * The numeric sort will **not** work on numbers written in exponential/scientific notation
  * `gEnA-` - sort rows ascending by the numerical value of the fifth column; in the case where values in the fifth column are equal, sort by the lexicographic value of the first column, descending.
* `u`: unique sorted rows
  * `$ ni <data> fACgABu` -- get the lexicographically-sorted unique values from the first and third columns of `<data>`.
* `c`: count sorted rows
  * `$ ni <data> fBgc` -- return the number of times each unique value of the second column occurs in `<data>`
  * Note that the above operation is superior to `$ ni <data> gBfBc` (which will give the same results), since the total amount of data that needs to be sorted is reduced.
  
##Perl for `ni` Fundamentals
`ni` fully supports Perl 5, and many of the operations can be written without quoting the environment. 

Note that whitespace is required after every p'code' operator; otherwise ni will assume that everything following your quoted code is also Perl.


* Returning data 
  * `p'..., ..., ...'`: Print each comma separated expression to its own row
  * `p'r ..., ..., ...'`: Print all comma separated expressions to one tab-delimited row to the stream
* Basic Field selection operations
  * `a`, `a()` through `l` and `l()`
  * `p'F_'`
  * `a_` through `l_`
  * `FR` 


##Intermediate Row and Column Operations
We can weave together row, column, and Perl operations to create more complex row operations. We also introduce some more 

* `r` - Take rows
  * `$ ni <data> rCF` - take rows where columns 3 and 6 are nonempty.
  * `$ ni <data> rp'<...>'` - take rows where the Perl snippet `<...>` is truthy in Perl. 
    * Be careful using `rp'...'` with numeric values, because the value `0` is not truthy in Perl; `$ ni n10 p'r a, 0' rpb` returns an empty stream.  
* `w`: Append column to stream
  * `$ ni <data> w[np'a*a']`
* `W`: Prepend column stream
  * `$ ni <data> Wn` - Add line numbers to the stream (by prepending one element the infinite stream `n`)
* `v`: Vertical operation on columns
  * **Important Note**: As of 2016-12-21, this operator is too slow to use in production. most of this 

##Useful Syntactic Sugar
* `o` and `O`: Numeric sorting
  * `o`: Sort rows ascending (numerical)
    * `oA` is syntactic sugar for `$ ni <data> gAn`
  * `O`: sort rows descending (numerical)
    * `OB` is equivalent to `$ ni <data> gBn-` 
  * **Important Note**: `o` and `O` sorts *cannot be chained together* or combined with `g`. Beacuse of this, there is no guarantee that the output of `$ ni <data> gAoB` will have a lexicographically sorted first column, and there is no guarantee that `$ ni <data> oBOA` will have a numerically sorted second column (and they will, with very high probability, not be sorted). 

##Perl for `ni`
A few important operators for doing data manipulation in Perl. Many Perl functions can be written without parentheses and 


* `lc()`
* `uc()`
* `substr()`
* `split`
* `join`
* `**`: exponent
* `$<expr>`: get the scalar value associated with `<expr>`
* `%<var> = `: construct a hash


##`ni`-specific Perl Extensions
The operators in this section refer specifically to the 
`$ ni <data> p'...'`

* Geohashing operations
  * `ghe`: geohash encoding
  * `ghd`: geohash decoding
* Time operations
  * `tpe`: time parts to epoch
  * `tep`: time epoch to parts
* Column operations
* Building hashes

##HDFS I/O & Hadoop Streaming

We'll assume some familiarity with HDFS (or access to someone with enough familiarity) 

* Reading from HDFS
  * `hdfs://<path>`: `hadoop fs -cat <path>`
  * `hdfst://<path>`: `hadoop fs -text <path>`
* Reading 

##Intermediate Perl Operations
###Begin Blocks
`p'^{BEGIN_BLOCK} ...'`



##Data Closures & Checkpoints

##Advanced Perl Operations
###Streaming Reduce
###Building Hashes


##Horizontal Scaling
Note that you will need sufficient processing cores to effectively horizontally scale. If your computer has 2 cores and you call `S8`, it may slow your work down.

* `S` -- Horizontal Scaling 




##Cell Operations 
`$ ni <data> ,<op><columns>`

* `,s` - running sum
* `,d` - difference between consecutive rows
* `,a` - running average
* `,z` - intify
* `,h` - murmurhash
* `,l` - natural log
* `,e` - natural exponential
* `,j` - jitter
* `,q` - quantize

##Stream Splitting/Joining/Duplication

* `+`: append a stream to this one
* `^`: prepend a stream to this one
* `%`: duplicate this stream through a process, and include output
* `=`: duplicate this stream through a process, discarding its output

   
##Advanced Row and Column Operations
* `j` - streaming join
  * Note that this join will only 
* `Y` - dense-to-sparse transformation
  * Explodes each row of the stream into several rows, each with three columns:
    * The first element of the output row is the index of row of the input data that came fro
    * The second element of the output row is the index of the column that the input data came from
    * The third element of the output row 
* `X` - sparse-to-dense transformation
  * `X` inverts `Y`; it converts a specifically-formatted 


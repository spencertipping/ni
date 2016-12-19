#Cheatsheet

`ni` is cheating already, so consider this a meta-cheatsheet of `ni` operations.


`$ni ...`

##Basic I/O
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
* `... > filename`: Redirect to file
  * Redirects the stream to `filename`, emits nothing
  * This is not a `ni` operation, just a file redirect.
* `... \>filename`: Write and emit filename:
  * Writes the stream to the file named `filename`, and emits the filename.
  * Note that there is no whitespace between the angle-bracket and the filename.
* `e[<script>]`: Evaluate Script
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
  * `$ ni <data> rCF` - take rows where columns 3 and 6 are nonempty.
  * `$ ni <data> r/<regex>/` - take rows where `<regex>` matches.
  * `$ ni <data> rp'<...>'` - take rows where the Perl code `<...>` is true.
* `g` - sort rows ascending (alphabetical)
* `o` - sort rows ascending (numerical)
* `O` - sort rows descending (numerical)
* `u` - unique sorted rows
* `c` - count sorted rows
*  `p'<...>'`: Perl
   * applies the Perl snippet `<...>` to each row of the stream 
*  `m'<...>'`: Ruby
   * applies the Ruby snippet `<...>` to each row of the stream 
*  `l'<...>'`: Lisp
   * applies the Lisp snippet `<...>` to each row of the stream 

##Basic Column Operations
* `f`: Take columns
  * `$ ni <data> fAA` - select the first column and duplicate it.
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

* `x` exchange columns
  * `x` -- exchange the first two columns. 
    * Equivalent to `fBA.`
  * `xC` -- exchange column 3 with column 1. 
    * Equivalent to `fCBA.`
  * `xBE` -- exchange columns 2 and 5 with columns 1 and 2. 
    * This runs in order, so `B` will be switched with `A` first, which will then be switched with column `E`. 
    * Equivalent to `fBECDA.`
* `w<...>`: append stream defined by `<...>` to each row in the stream
* `W<...>`: prepend stream defined by `<...>` to each row in the stream

  
##Basic Perl Operations
`ni` fully supports Perl 5.

###`ni`-specific Perl Extensions
The operators in this section refer specifically to the 
`$ ni <data> p'...'`

* Column operations
* Time operations
* Geohashing operations
* Building hashes

###Begin Blocks
`p'^{BEGIN_BLOCK} ...'`


##HDFS I/O & Hadoop Streaming


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



#Cheatsheet
###...well, it was before it got out of hand.
######(I'm working on it.)

`ni` is cheating already, so consider this a meta-cheatsheet of `ni` operations.

`$ni ...`


##Input Operations
* `n`: Integer stream
  * `n#`: Stream the sequence `1..#`, one number per line
  * `n0#`: Stream the sequence `0..#-1`, one number per line
  * `n`: Stream the sequence `1..`, one number per line.
* `filename{, .gz, .bz, .xz, .lzo, .txt, .csv, .json, etc}`: File input
  * Automatically decompresses the file and streams out one line of the file out at a time.
* `e[<script>]`: Evaluate script
  * evaluate `<script>` in bash, and stream out the results one line at a time.
* `id:<text>`: Literal text input
  * `$ ni id:OK!` -- add the literal text `OK!` to the stream.
  * `$ ni id:'a cat'` -- add the literal text `a cat` to the stream. The quotes are necessary to instruct the `ni` parser where the boundaries of the string are. Double quotes will work as well.
* `input_directory \<`: Read from directory
  * This tool can be powerful in combination with Hadoop operations, described below.
* `D:<field1>,:<field2>...`: JSON Destructure
  * `ni` implements a very fast (but not quite complete) JSON parser,

##File Output and Compression
* ` > filename`: Redirect stream to file
  * Redirects the stream to `filename`, emits nothing
  * This is not a `ni` operation, just a file redirect.
* ` \>filename`: Redirect stream to file and emit filename
  * Consumes the stream and outputs it to the file named `filename`, and emits the filename.
  * Note that there is no whitespace between the angle-bracket and the filename.
* `=\>filename`: Duplicate stream to file
  * This operation combines the `=` operator (described below) with `\>filename`.
  * Whereas `\>` will consume your entire stream, ending any future processing, `=\>` duplicates the stream, sending one version to a file, while the other can continue to be processed.
* `z`: Compress stream
  * Defaults to applying `gzip` compression, but can use any number of operations. 
  * `zg`: explicit gzip with default compression
  * `zg9`: gzip with compression level 9
  * `zn` writes all output to `dev/null`; useful for forcing an operation to complete.
  * `zo`: lzo compression
  * `zx`: xzip compression
  * `zb`: bzip2 compression
  * `z4`: lz4 compression (note, some Docker images have a too-old version of LZ4)


##Basic Row Operations
* `r`: Take rows
  * `$ ni <data> r3` - take the first 3 rows of the stream
  * `$ ni <data> r-3` - take everything after the first 3 rows of the stream
  * `$ ni <data> r~3` - take the last 3 rows of the stream
  * `$ ni <data> r100x` - take every 100th row in the stream
  * `$ ni <data> r.05` - sample 5% of the rows in the stream.
    * The sampling here is deterministic (conditioned on the environment variable `NI_SEED`) and will always return the same set of rows.
  * `$ ni <data> r/<regex>/` - take rows where `<regex>` matches.
*  `p'<...>'`: Perl
   * applies the Perl snippet `<...>` to each row of the stream 
   * `p'..., ..., ...'`: Prints each comma separated expression to its own row into the stream.
* Combining `p'...'` with `r`
  * `r` can take `p'...'` as an arugment, forming the operator `rp'...'`
  * `$ ni <data> rp'<...>'` - take rows where the Perl snippet `<...>` is truthy in Perl. 
  * Be careful using `rp'...'` with numeric values, because `0` is not truthy in Perl; `$ ni n10 p'r a, 0' rp'b'` returns an empty stream. 

##Basic `ni` Philosophy and Style

####`ni` is written for concision, fast testing, and productivity.

* Because `ni` processes streams of data in a streaming way, you should be able to build pipelines up from front-to-back, checking the integrity of the output at each step, for example by taking some rows using `r30` and/or sinking the output to a file.
* As a result of this, `ni` spells can be developed completely in the command line; there is:
  * No compilation (in the most common sense).
  * No need for a text editor.
  * No need to switch windows.
  * Nothing stopping you from joyful hacking.
* If you find a common operation is taking a while, there's probably a faster way to do it. Ask. It may require more Perl, though.

####Everything in `ni` is optional *unless its absence would create ambiguity*.

* Starting with an example:
  * Here is a clear `ni` spell: `ni n10 rp'a > 3'`
  * Here is the same spell, written compactly: `ni n10rpa\>3`
  * Escaping characters to avoid quoting probably isn't the best thing to do, but to an experienced `ni` developer, both of these spells will be about equally readable.
* Here's another:
  * Clear: `ni n10p'r "hello"'`
  * Conciser: `ni n10p'hello'`
  * Compact: `ni n10phello`
  * The technical details behind the `ni` parser are out of scope, but 


####`ni` is not meant to be easy to read for those who do not speak `ni`.

* The `ni` learning curve is rather steep; if you come to `ni` from a Python/Ruby background, with their almost English-like syntax, you may find `ni` unfriendly at first. 
* However, just as most `ni` spells are built front-to-back, `ni` spells can be understood back-to-front. By repeatedly clipping operations off the end, you can see the entire 
* And moreover, `ni` is magic. Magic is not meant to be understood by the uninitiated. That's why wizards live in towers and why `ni` snippets are  properly referred to as spells.


##Basic Column Operations
Columns are referenced "Excel-style"--the first column is `A`, the second is `B`, etc.

* `f`: Take columns
  * `$ ni <data> fAA` - select the first column and duplicate it
  * `$ ni <data> fAD.` - select the first column and all remaining columns starting with the fourth
  * `$ ni <data> fB-E` - select columns 2-5
  * `$ ni <data> fCAHDF` - selects columns 3, 1, 8, 4, 6, and streams them out in that order.
* Combining column operations with `r`
  * `$ ni <data> rCF` - take rows where columns 3 and 6 are nonempty.
* `F`: Split stream into columns
  * `F:<char>`: split on character
    * Note: this does not work with certain characters that need to be escaped; use `F/regex/` below for more flexibility (at the cost of less concision).
  * `F/regex/`: split on occurrences of regex. If present, the first capture group will be included before a tab is appended to a field.
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

Note that whitespace is required after every `p'code'` operator; otherwise ni will assume that everything following your quoted code is also Perl.

* Returning data 
  * `p'r ..., ..., ...'`: Print all comma separated expressions to one tab-delimited row to the stream
  * `p'[..., ..., ...]'`: Return each element of the array as a field in a tab-delimited row to the stream.
* Field selection operations
  * `a`, `a()` through `l` and `l()`: Parsimonious field access
    * `a` through `l` are functions that access the first through twelfth fields of an input data stream. They are syntactic sugar for the functions `a()` through `l()` with the same functionality.
    * Generally, the functions `a` through `l` will be more parsimonious, however, in certain contexts (importantly, this includes hash lookup), these one-letter functions will be interpreted as strings; in this case, you must use the more explicit `a()` syntax. 
  * `F_`: Explicit field access
    * Useful for accessing fields beyond the first 12, for example `$ ni <data> F_ 6..15`
    * `FM` is the number of fields in the row.
    * `FR n` is equivalent to `F_ n..FM`
 

##JSON Encoding

*  `p'json_encode {<row to JSON instructions>}`: JSON Encode
  *  The syntax of the row to JSON instructions is difficult; I believe `ni` will try to interpret value as a `ni` command, but every other unquoted piece of text will be interpreted as 
  *  Here's an example:
```
ni //license FWpF_ p'r pl 3' \
     p'json_encode {type    => 'trigram',
                    context => {w1 => a, w2 => b},
                    word    => c}' \>jsons```

##Intermediate Column Operations
We can weave together row, column, and Perl operations to create more complex row operations. We also introduce some more advanced column operators.

* `w`: Append column to stream
  * `$ ni <data> w[np'a*a']`
* `W`: Prepend column stream
  * `$ ni <data> Wn` - Add line numbers to the stream (by prepending one element the infinite stream `n`)
* `v`: Vertical operation on columns
  * **Important Note**: As of 2016-12-23, this operator is too slow to use in production.

##Perl for `ni`
A few important operators for doing data manipulation in Perl. Many Perl subroutines can be written without parentheses or unquoted directly in to `ni` scripts. Go look these up in docs online until something more substantial is written here.

* `lc`
* `uc`
* `substr`
* `split`
* `join`
* `**`: exponent
* `my $<v> = <expr>`: instantiate a scalar `<v>` with the value of `<expr>`
* `map`
* `keys %h`
* Regular Expressions
  * `$<v> =~ /regex/`
  * `$<v> =~ s/regex//`
  * `$<v> = tr/regex//d`
  * `$<v> = y/regex//`


##Useful Syntactic Sugar
* `o` and `O`: Numeric sorting
  * `o`: Sort rows ascending (numerical)
    * `oA` is syntactic sugar for `$ ni <data> gAn`
  * `O`: sort rows descending (numerical)
    * `OB` is equivalent to `$ ni <data> gBn-` 
  * **Important Note**: `o` and `O` sorts *cannot be chained together* or combined with `g`. There is no guarantee that the output of `$ ni <data> gAoB` will have a lexicographically sorted first column, and there is no guarantee that `$ ni <data> oBOA` will have a numerically sorted second column.  With very high probability, they will not be sorted.

##Useful `ni`-specific Perl Subroutines
The operators in this section refer specifically to the 
`$ ni <data> p'...'`

* `ghe`: geohash encoding
  * `ghe($lat, $lng, $precision)`
    * If `$precision > 0`, returns a geohash with `$precision` base-32 characters of precision. 
    * If `$precision < 0`, returns a geohash with `$precision` (base-2) bits of precision.
* `ghd`: geohash decoding
  * `ghd($gh_base32)`
     * Returns the corresponding latitude and longitude (in that order) of the southwesternmost point corresponding to that geohash.
  * `ghd($gh_int, $precision)`
    * If the number of bits of precision is specified, `ghd` will decode the input integer as a geohash with $precision bits. Returns the  latitude and longitude (in that order) of the southwesternmost point corresponding to that geohash.
* `tpe`: time parts to epoch
  * `tpe(@time_pieces)`: Returns the epoch time and assumes that the pieces are year, month, day, hour, minute, and second, in that order.
  * `tpe($time_format, @time_pieces)`: Returns the epoch time, using `$time_format` to determine what the ordered `@time_pieces` are.
* `tep`: time epoch to parts
  * `tep($epoch_time)`: returns the year, month, day, hour, minute, and second in human-readable formatfrom the epoch time.
  * `tep($time_format, $epoch_time)`: returns the specified parts of the date using following `$time_format`.
* `timezone_seconds`
  * `tep($raw_timestamp + $timezone_seconds($lat, $lng))` returns the approximate date and time at the location `$lat, $lng` at a Unix timestamp of `$raw_timestamp`.
  
##Data Closure Basics and Array-Based Operators
Data closures are useful in that they travel with `ni` when `ni` is sent somewhere else, for example over ssh, or as a jar to a Hadoop cluster. Importantly, closures can be accessed from within Perl snippets by using their name.

* `closure_name::[...]`: Create a data closure
  * Any legal `ni` snippet that is executable on the machine from whose context `ni` is being executed.
  * The closure can be referenced within a Perl snippet as  `p' ... closure_name ...'`
* `a_` through `l_`: Array-based line operations 
  * Data closures are transferred as an array of lines; in order to access data from a specific column of the data closure, you will need to use array-based line operators `a_` through `l_`, which are the array-based analogs to the line-based operators `a/a()` through `l/l()`.
  * `ni ::data[n1p'cart [1,2], [3,4]'] n1p'a_ data'` works, because `a_` is operating on each element of the array.
  * `ni ::data[n1p'cart [1,2], [3,4]'] n1p'a(data)'` and `ni ::data[n1p'cart [1,2], [3,4]'] n1p'a data'` will raise syntax errors, since `a/a()` are not prepared to deal with the more than one line data in the closure.

##HDFS I/O & Hadoop Streaming
###How `ni` Interacts with Hadoop Streaming
When `ni HS...` is called, `ni` packages itself as a `.jar` to the configured Hadoop server, which includes all the instructions for Hadoop to run `ni`.

When `ni` uploads itself, it will also upload all data that is stored in data closures; if these closures are too large, the Hadoop server will refuse the job.

###Hadoop Operators
* `hdfs://<path>`: HDFS `cat`
  * Equivalent to `hadoop fs -cat <path>`
* `hdfst://<path>`: HDFS `text`
  * Equivalent to `hadoop fs -text <path>`
* `HS[mapper] [combiner] [reducer]`: Hadoop Streaming Job
  * Any `ni` snippet can be used for the mapper, combiner, and reducer. Be careful that all of the data you reference is available to the Hadoop cluster; `w/W` operations referencing a local file are good examples of operations that may work on your machine that may fail on a Hadoop cluster with no access to those files.
  * `_` -- skip the mapper/reducer/combiner. 
  * `:` -- apply the trivial operation (i.e. redirect STDIN to STDOUT) for the mapper/reducer/combiner
  * If the reducer step is skipped with `_`, the output may not be sorted, as one might expect from a Hadoop operation. Use `:` for the reducer to ensure that output is sorted correctly.
  * Remember that you will be limited in the size of the `.jar` that can be uploaded to your Hadoop job server; you can upload data closures that are large, but not too large.
* Using HDFS paths in Hadoop Streaming Jobs:
  * `ni ... \'hdfst://<path> HS...`
  * The path must be quoted so that `ni` knows to get the data during the Hadoop job, and not collect the data, package it with itself, and then send the packaged data as a `.jar`.
 
##Intermediate `ni` Philosophy and Style
####`ni` is a domain-specific language; `ni`'s domain is processing single lines and chunks of data that fit in memory


* Because of this philosophy, `ni` is fantastic for data munging and cleaning.
* Because of this philosophy, large-scale sorting is not a `ni`-ic operation, while compression is.
* Because of this philosophy, `ni` relies heavily on Hadoop for big data processing. Without Hadoop, most sufficiently complicated operations become infeasible from a runtime perspective once the amount of data exceeds a couple of gigabytes, uncompressed. 


####What `ni` doesnt do.
  * `ni` isn't meant to replace your heavy-featured easy-to-read scripting language (completely... yet); just all of the operations that aren't heavily-optimized and complicated math.
  * If you're coming from a heavy-featured scripting language, try to take joy in the speed of development that `ni` provides. `ni`'s row and column selection operations are much easier to discuss than `pandas`' `.loc` and `.ix`, for example.
  


##Basic Perl Reducers

The operations here are generally dependent on sorting to function properly, which can make them very expensive to execute on a single machine.

###Streaming Reduce
These operations encapsulate the most common types of reduce operations that you would want to do on a dataset; if your operation is more complicated, it may be more effectively performed using the buffered readahead and line-array reducers.

* `sr`: Reduce over entire stream
  * `$ ni n1E5p'sr {$_[0] + a} 0'`: sum the integers from 1 to 100,000
* `se`: Reduce while equal
  * `@final_state = se {reducer} \&partition_fn, @init_state`
* `sea` through `seq`: Reduce with partition function `a()...q()`
* `rc`: Compound reduce
* `rfn`: Custom compound reduce

##Cell Operations 
`$ ni <data> ,<op><columns>`

These provide keystroke-efficient ways to do transformations on a single column of the input data. Of particular use is the deterministic hashing function, which does a good job of compacting long IDs into 32-bit integers. With ~40 million IDs, there will be only be about 1% hash collisions, and with 400 million IDs, there will be 10% hash collisions.  See [this](http://math.stackexchange.com/questions/35791/birthday-problem-expected-number-of-collisions) for why.

* `,a`: Running average
* `,d`: Difference between consecutive rows
* `,e`: Natural exponential (`e^x`)
* `,h`: Murmurhash (deterministic 32-bit hash function)
* `,j<amt>`: Jitter (add uniform random noise in the range `[-amt/2, amt/2]`)
* `,l`: Natural log (`ln x`)
* `,s`: Running sum 
* `,q`: Quantize
* `,z`: Intify (hash and then convert hash values to integers starting with 1)


##Horizontal Scaling
Note that you will need sufficient processing cores to effectively horizontally scale. If your computer has 2 cores and you call `S8`, it may slow your work down, as `ni` tries to spin up more processes than your machine can bear.

* `S`: Horizontal Scaling 
  * `$ ni <data> S<# cores>[...]`: distributes the computation of `...` across `<# cores>` processors.

##Advanced Perl Operations
* `p'^{BEGIN_BLOCK} ...'`: Begin block
  * A begin block is indicated by attaching a caret (`^`) to a block of code (encolsed in `{ }`). Outside of begin blocks, the Perl code is evaluated for every row; inside a begin block, the code is evaluated once and this evaluation takes precedence over 
  * Begin blocks are useful for converting data closures to Perl data structures, and defining other constants that are used in 
  These blocks are evaluated in their entirety 
* `p'%h = <col_1><col_2>_ @lines`: Hash constructor
  * Hash constructors are useful for filtering large datasets without having to invoke an expensive sort or an HDFS join. Hash constructors are useful inside of begin blocks, often using the following workflow:
    * Generate a list of things you want to filter, and put it in a data closure. `::ids[list_of_ids]`
    * Convert the data closure to a hash using a begin block (`^{%id_hash = ab_ ids}`)
    * Filter another dataset (`ids_and_data`) using the hash (`exists($id_hash{a()})`)
    * `ni ::ids[list_of_ids] ids_and_data rp'^{%id_hash = ab_ ids} exists($id_hash{a()})'` 


##Advanced Perl Reducers
###Buffered Readahead
These operations are good for reducing 

* `rw`: read while
  * `@lines = rw {condition}`: read lines while a condition is met
* `ru`: read until
  * `@lines = ru {condition}`: read lines until a condition is met
* `re`: read equal
  * `@lines = re {condition}`: read lines while the value of the condition is equal.

###Line-Array Reducers
These operations can be used to reduce the data output by the readahead functions. Look at the input provided by the first perl statement, 

* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'sum b_ re {a}'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'sum a_ re {b}'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r all {a_($_)} re {b}'`
* `ni n1p'cart ["a", "a", "b", "c"], [1, 2]' p'r uniq a_ re {b}'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r maxstr a_ re {b}'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r reduce {$_ + a} 0, re{b}` <- DOES NOT WORK BECAUSE BILOW WROTE IT WRONG

##Stream Splitting/Joining/Duplication
I don't find these operators particularly useful in practice (with the exception of `=\>` for writing a file in the middle of a long processing pipeline), but it's possible that you will! Then come edit these docs and explain why.

* `+`: append a stream to this one
* `^`: prepend a stream to this one
* `%`: duplicate this stream through a process, and include output
* `=`: duplicate this stream through a process, discarding its output


##Advanced Row and Column Operations
* `j` - streaming join
  * Note that this join will consume a single line of both streams; it does **NOT** provide a SQL-style left or right join.
* `Y` - dense-to-sparse transformation
  * Explodes each row of the stream into several rows, each with three columns:
    * The index of the row that the input data that came from
    * The index of the column that the input data came from
    * The value of the input stream at the row + column specified by the first two columns.
* `X` - sparse-to-dense transformation
  * `X` inverts `Y`; it converts a specifically-formatted 3-column stream into a multiple-column stream.
  * The specification for what the input matrix must look like is described above in the `Y` operator.
  
  

##Matrix Operations
These operations are suited best to 


* `N'x = ...'`: Numpy-style matrix operations
  * Dense matrices can be transformed using Numpy-like operations
  * The entire input matrix (i.e. the stream) is referred to as `x`.
  * Example: `ni n10p'r map a*$_, 1..10' N'x = x + 1'` creates a matrix and adds one to every element with high keystroke efficiency.
  * Example `ni n10p'r map a*$_, 1..10' N'x = x.T'`
* `X<col>`, `Y<col>`, `N<col>`: Matrix Partitioning
  * **TODO**: understand how these actually work.
* `X`: sparse-to-dense transformation
  * In the case that there are collisions for locations `X`, `X` will sum the values
  * For example: `ni n010p'r 0, a%3, 1' X`

##Advanced Data Closures & Checkpoints

* `::closure_name[...]`
  * Regardless of where they are written in the `ni` command, data closures are computed before anything else, including Perl begin blocks. They are also computed separately from each other, which means that one closure cannot in general reference the value of another closure.
  * If the value of one closure depends on the value of another, the other closure must be computed within the first closure; this leads to duplication of code. It's not the best, but if you need this, you're probably using `ni` wrong too.
* `@:[disk_backed_data_closure]`
* `:[checkpoint]`

##Annoyingly Advanced Perl
* `use strict` and the `::` prefix in a Perl Environment
  * When `use strict` is enabled, Perl will complain when you try to create a variable in a Perl snippet that does not start with `::`.
  * The reasons for this are very specific to Perl; if you are a true Perl nerd, you can look them up, but you do not need to know them if you just accept that variables need to start with `::` when you apply `use strict`.
  * It is probably a good idea to `use strict` when the variables you define are sufficiently complex; otherwise you're probably okay not using it.
  
##Plotting with `ni --js`

##Less Useful `ni`-specific Perl Extensions
* Array functions
  * `clip`
  * `within`


##Things other than Perl 

Look, these are here, and if it helps you get started with `ni`, great. But `ni` is written in Perl, for Perl, and in a Perlic style. Use these, but go learn Perl.

*  `m'<...>'`: Ruby
   * applies the Ruby snippet `<...>` to each row of the stream 
*  `l'<...>'`: Lisp
   * applies the Lisp snippet `<...>` to each row of the stream 
   
##Writing Your Own `ni` Extensions
**TODO** Understand how this works

##Obscure Interops
Here be dragons, and files that start with `<<EOF`.

* [SQL](sql.md)
* [PySpark](pyspark.md)
* [Scripting](script.md)
* 
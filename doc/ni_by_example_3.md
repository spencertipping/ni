#`ni` by Example, Chapter 3 (WIP)

Welcome to the third part of the tutorial. At this point, you've probably activated a certain amount of `ni` productivity by using Hadoop to broadcast your `ni` scripts across hundreds of cores and thousands of jobs.  You should also have a reasonable high-level understanding of how `ni` is a self-modifying quine, and how this allows it to execute across operating systems.

In this chapter, our goal is to multiply your power by introducing important Perl operations including reducers, cell operations, and ni-specific functions. We'll also demonstrate `ni`'s connections to Python and numpy, Ruby, and Lisp. And for the time being, we'll show how to patch one of `ni`'s weaknesses (large-scale joins) using `nfu`.

However, we'll first codify `ni` philosophy and style, which we've touched on in the previous two chapters.



##`ni` Philosophy and Style

####`ni` demands expertise

####Conciseness matters; readability to the uninitiated does not.
`ni` spells should be beautiful

####Outsource hard jobs to more appropriate tools.
The most obvious 

Some jobs that are difficult for `ni`:
* Sorting
* Matrix Multiplication
* SQL Joins

Here's how `ni` solves those:







##Perl for `ni` fundamentals



##Connecting `bash` and `ni`

* `e[<script>]`: Evaluate script
  * evaluate `<script>` in bash, and stream out the results one line at a time.



  
##Understanding the `ni` monitor
More details [here](monitor.md). Overall:

* Negative numbers = non-rate-determining step
* Positive numbers = rate-determining step
* Large Positive numbers = slow step







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
  
  
##Plotting with `ni --js`
Check out the [tutorial](tutorial.md) for some examples of cool, interactive `ni` plotting.

**TODO**: Say something useful.


##Multiline Selection Operators


* `a_` through `l_`: Multiline Selection operations 
  * Data closures are transferred as an array of lines; in order to access data from a specific column of the data closure, you will need to use multiline operators `a_` through `l_`, which are the multilineanalogs to the line-based operators `a/a()` through `l/l()`.
  * `ni ::data[n1p'cart [1,2], [3,4]'] n1p'a_ data'` works, because `a_` is operating on each element of the array.
  * `ni ::data[n1p'cart [1,2], [3,4]'] n1p'a(data)'` and `ni ::data[n1p'cart [1,2], [3,4]'] n1p'a data'` will raise syntax errors, since `a/a()` are not prepared to deal with the more than one line data in the closure.

 
##Intermediate `ni` Philosophy and Style
####`ni` is a domain-specific language; its domain is processing single lines and chunks of data that fit in memory

* Because of this philosophy, `ni` is fantastic for data munging and cleaning.
* Because of this philosophy, large-scale sorting is not a `ni`ic operation, while gzip compression is.
* Because of this philosophy, `ni` relies heavily on Hadoop for big data processing. Without Hadoop, most sufficiently complicated operations become infeasible from a runtime perspective once the amount of data exceeds a couple of gigabytes, uncompressed.

####What `ni` doesnt do.
* `ni` is adequate at arithmetic and line-based function application, but generally `ni` is bad at math. 


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
* `,e`: Natural exponential (`e**x`)
* `,h`: Murmurhash (deterministic 32-bit hash function)
* `,j<amt>`: Jitter (add uniform random noise in the range `[-amt/2, amt/2]`)
* `,l`: Natural log (`ln x`)
* `,s`: Running sum 
* `,q`: Quantize
* `,z`: Intify (hash and then convert hash values to integers starting with 1)


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

###Multiline Reducers
These operations can be used to reduce the data output by the readahead functions. Look at the input provided by the first perl statement, 

* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'sum b_ re {a}'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'sum a_ re {b}'`

`rea` is the more commonly used shorthand for `re {a}`

* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r all {a_($_)} reb'`
* `ni n1p'cart ["a", "a", "b", "c"], [1, 2]' p'r uniq a_ reb'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r maxstr a_ reb'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r reduce {$_ + a} 0, reb` <- DOES NOT WORK BECAUSE BILOW WROTE IT WRONG

##Stream Splitting/Joining/Duplication
I don't find these operators particularly useful in practice (with the exception of `=\>` for writing a file in the middle of a long processing pipeline), but it's possible that you will! Then come edit these docs and explain why.

* `+`: append a stream to this one
* `^`: prepend a stream to this one
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

Operations on huge matrices are not entirely `ni`ic, since they may require space greater than memory, whichwill make them slow. However, operators are provided to improve These operations are suited best to 


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
  
##Binary Operations
In theory, this can save you a lot of space. But I haven't used this in practice.
  

##Less Useful `ni`-specific Perl Extensions
###JSON Encoding

*  `p'json_encode {<row to JSON instructions>}`: JSON Encode
  *  The syntax of the row to JSON instructions is difficult; I believe `ni` will try to interpret value as a `ni` command, but every other unquoted piece of text will be interpreted as 
  *  Here's an example:
```
ni //license FWpF_ p'r pl 3' \
     p'json_encode {type    => 'trigram',
                    context => {w1 => a, w2 => b},
                    word    => c}' \>jsons```


###Array Functions
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

##Obscure Interops/Extensions
Here be dragons, and files that start with `<<EOF`.

* [SQL](sql.md)
* [PySpark](pyspark.md)
* [Scripting](script.md)
* [HTTP Operations](net.md)
* [Defining `ni` functions](fn.md)
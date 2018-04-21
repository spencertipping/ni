# `ni` Perl Cheatsheet (alpha release)

`ni` fully supports Perl 5 with backwards compaitibility to 5.08, and most of your `ni` scripts likely should be written in Perl. 

`$ ni data p'<...>'` applies the Perl snippet `<...>` to each row of the stream.

## Printing and Returning Data
  * `p'r ..., ..., ...'`: Print all comma separated expressions to one tab-delimited row to the stream.
  * `p'<statements>; <$val1>, <$val2>, ...'`: Returns each element `<$val1>`, `<$val2>`,... on its own line.
  * Examples:
      * `ni 1p'20, 30'` returns `20` and `30` on separate lines.
      * `ni 1p'r 20, 30'` returns `20	30` on a single, tab-separated line.
      * Recall that `1` is the same thing as `n1`.
  * The `p'...; r ..., ...'` operator is used more frequently, 
  * There are some tricks to how `ni` prints data from references.
  * For example:
      * `$ ni 1p'[hi, there]'` returns `hi	there` on a single tab-separated line:
      * `$ ni 1p'r [hi, there]'` prints `ARRAY(0x7fa31dbc5b60)`.



## Basic Perl Syntax

### Data Types

Perl has a relatively small number of data types, and for the purpose of this cheatsheet you really only need to know about these three:

* Scalars -- numbers, strings
* Arrays -- Arrays are written with parentheses
* Hashes -- 
* References: References are in fact just a special type of scalar used to refer to one of the other data types
  * Array references are written inside square brackets
  * Hash references are written inside curly braces

### Sigils

Perl variables are indexed with "sigils" to signify their type. This can be a little tricky for Perl beginners, so let's review quickly.

* `$x` is a scalar (a number, string, or reference to one of the other types).
* `@x` is an array.
* `$x[0]` is a scalar (its sigil is `$`), whose value is the first element of the array `@x`. Perl knows that you're referring to an the array `@x` because you're indexing with square brackets.
* `%h` is a hash.
* `$h{"bar"}` is a scalar (its sigil is `$`), and it is the value of `%h` associated with the key `"bar"`. Perl nows you're referring to the hash `%h` because you're indexing it with curly braces.
* Because sigils make the langauge very explicit, it is possible (though not advisable) to have a hash `%u`, an array `$u`, and a scalar `$u`, which have nothing to do with each other.
* `foo` (without a preceding sigil) is called a "bareword." There are Parsing rules for barewords are If there is a function with no arguments called `foo`, 


### Caveats

* There are no key errors and no index errors in Perl
* If you try to reference a key or index that does not exist, the hash or array will automatically create 
* If you try to operate on an undefined value, it will define itself to be compatible with the operation you use and default to zero, the empty string, the empty array/hash, etc.


### Lexical scoping

Perl is lexically scoped; in general, when using `ni`, you will want to prefix every variable you define within a Perl mapper as scoped to the block, using the keyword `my`, as in `$ ni n10 p'my $x = 0; r $x'`

The exception to this rule is for variables defined within a begin block. In this case `my` is not used at all. See: `$ ni n10 p'^{$x = 0} $x += 1; r $x'`.

### Reference Basics

* To convert an object to a reference, use a backslash.
* Example: `my $x = "nice ref!"; my $xref = \$x; r $xref` will print something like `SCALAR(0x7fc809a8ed20)`. 
* To convert a reference back to a particular value type, wrap the reference in curly braces and prepend with the correct sigil.
* Example: `$ ni 1p'my $x = "nice ref!"; my $xref = \$x; r ${$xref}'` prints `nice ref!`.
* The curly braces are not always necessary, so you may also see confusing-at-first-blush compound sigils, as in: `$ ni 1p'my $x = "nice ref!"; my $xref = \$x; r $$xref'` which also prints `nice ref!`.
* Dereferencing is time-consuming and can significantly degrade performance when used carelessly.

  
## Field Selection

### `a` through `l`: Short field access
  * `a` through `l` are functions that access the first through twelfth fields of an input data stream. 
  * `$ ni i[one two three four] p'r d, b'` prints `four	two` to the output stream. 
* In the context of hash lookup, the functions `a` through `l` without parentheses will be interpreted as strings; in this case, you must use the more explicit `a()` syntax. 
  * `$h{a}` tries to retrieve the key associated with the string `"a"` from the hash `%h`.
  * `$h{a()}` or `$h{+a}` tries to retrieve the key associated with the value of the function `a`, _i.e._ the value in the first column.

```
$ ni ihi p'my %h = ("hi", "bye", "a", "eiou");
           r $h{a}, $h{+a}, $h{a()}'
``` 
returns `aeio	bye	bye`. Inside hash-lookup braces (among other places), prefixing a bareword with the `+` operator tells Perl to interpret it as a function call. This is useful in a number of contexts where 

### `F_`, `FM`, `FT`, `FR` : Explicit field access

* `F_`: returns the values of the tab-separated columns as an array.
  * `F_(@inds)` will return the values of 
* `FM`: returns the index of the largest defined index in `F_` (`== @r = F_; $#r`)
* `FR $n`, `FT $n`: `F_` fRom, `F_` To
  * `FR $n` returns `F_` starting from and including position `$n` to the end of `F_`. 
  * `FT $n` returns `F_` starting from index `0` up to and including index `$n-1`. 
  * `F_ == FT $n, FR $n`.

## `rp'...'`: Take rows with Perl

This operator combines the `r` operator from the [operator cheatsheet](cheatsheet_op.md) with Perl, and takes every line that evaluates to true.

* `$ ni <data> rp'<...>'` - take rows where the Perl snippet `<...>` returns a truthy value in Perl. 
* Be careful using `rp'...'` with numeric values, because `0` is falsey in Perl. `$ ni n10 p'r a, 0' rp'b'` returns an empty stream. 

## Important Perl Builtins

If you are unfamiliar with these functions, they are explained more extensively in [Chapter 2 of `ni` by example](ni_by_ex_2.md).


### String Utilities
* `lc`
* `uc`
* `substr`
* `split`
* `join`

### List Utilities
* `map`
* `grep`
* `keys`
* `values`
 
### Regular Expressions
* `$<v> =~ /regex/` -- standard regex form
* `$<v> =~ s/regex//` -- substitution
* `$<v> = tr/regex//` -- transliteration (also can be done with `y/regex//`)

### Logical and Existence Operators
* `exists` -- truthy when a key exists in a hash, falsey otherwise
* `defined` -- truthy when a value is not `undef`, falsey otherwise
* `&&`, `||` -- high priority boolean operators; use to perform boolean operations directly on scalar values 
* `//` -- "logical-defined OR"; `//` is identical to `||`, except it returns the first value if it is *defined* rather than if it is *truthy*.
* `&&`, `||`, and `//` return the last value evaluated, unlike the corresponding C operators, which return `0` and `1`.
* `and`, `or` -- low priority boolean operators; use to logically join compound statements


### BEGIN and END Blocks
* `p'^{<begin_block>} ...'`: Begin block
  * A begin block is indicated by attaching a caret (`^`) to a block of code (encolsed in `{ }`). Outside of begin blocks, the Perl code is evaluated for every row; inside a begin block, the code is evaluated once and this evaluation occurs before any other code in the mapper is evaluated.
  * Begin blocks are useful for converting data closures to Perl data structures, and defining things like constants and counters that should be maintained over runs with different rows.
* `p'...; END {<end_block>}'`
  * Similar to a begin block, these run only once the last line of the input stream has been processed. These are useful for emitting the value of counters and data structures that have been accumulated over all of the rows of a computation.



## Useful `ni`-specific Perl Subroutines


### Line Utilities

* Line Reading
	* `rl($n)`: read lines
	  * returns an array consisting of the next `$n` lines of the stream, including the current line.
	  * When `rl` is executed without an argument, the current line is ignored, the next line in the stream is added to the pushback queue. 
	* `pl($n)`: peek lines
	  * `pl` ignores the current line, peeks ahead `$n` lines until the global pushback queue is full. Once the pushback queue is full, it does not pull in additional lines, and returns the value of the pushback queue.
* Buffered Readahead
   * Whereas `rl` and `pl` tab split their data, the buffered readahead options do not.
	* `rw`: read while
	  * `@lines = rw {condition}`: read lines while a condition is met
	* `ru`: read until
	  * `@lines = ru {condition}`: read lines until a condition is met
	* `re`: read equal
	  * `@lines = re {condition}`: read lines while the value of the condition is equal.
	  * `@lines = re { b }` will put text lines into `@lines` where `b` is constant.


### List Utlities

* List Access
  * `first(@r)`: first element of `@r` (`== $r[0]`)
  * `final(@r)`: last element of `@r` (`== $r[-1]`)
  * `rando(@r)`: a random element of `@r`
* Math and Logic Operations
  * `max(@r)`, `min(@r)`: numeric `max` and `min`
  * `maxstr(@r)`, `minstr(@r)`: lexicographic `max` and `min`
  * `deltas(@r)`, `totals(@r)`: differences of consecutive elements, and running sum of elements.
  * `argmax(&block, @r)`, `argmin(&block, @r)`: returns the value of the element of `@r` that maximizes (minimizes) `&block`.
  * `indmax(&block, @r)`, `indmin(&block, @r)`: returns the index of the element of `@r` that maximizes (minimizes) `&block`.
  * `any(&block, @r)`, `all(&block, @r)`
     * Returns logical `or` (`and`) of `&block` applied to all of the elements of `@r`.
* Count and Unique
  * `uniq(@r)`: list of unique elements of `@r`
  * `freqs(@r)`: hash of the count of each unique element in `@r`
    * `freqs` returns a reference; it's commonly used as `p'...; my %h = %{freqs @r}; ...'`.
* Functional Programming
  * `take($n, @r)`, `drop($n, @r)`: take or drop the first `$n` elements of `@r` and return the result.
  * `take_while(&block, @r)`, `drop_while(@r, &block)`: takes or drops elements from `@r` while the block `&block` is true. 
  * `take_every($n, @r)`, `take_even(@r)`, `take_odd(@r)`: takes every `$n`th value, every value at an even index in the list, and every odd value of the list.
  * `reduce(&block, $start, @r)` executes 
     * Returns the result of iteratively applying `$start = &block($start, shift @r)`.
  * `reductions(&block, $start, @r)`
      * Returns a list consisting of all of the intermediate values computed in `reduce`.
* Cartesian Product
  * `cart`: Cartesian Product
* Functions on arrays of text lines
  * A number of functions described later (namely `re`, `ru`, `rw`) will read in lines of text without tab-splitting them.   
  * `a_`, `b_`, and others: get column from lines
     * 
  * `a__`, `b__`, and others: get many columns from lines
     * 

### Math Utilities
* `sum`, `prod`, `mean`, `log2`: standard math functions
* `quant($x, $q)`: rounds `$x` to the nearest `$q`.
* `dot`, `cross`: scalar and vector products
* `l1norm`, `l2norm`: L1 and L2 vector norms
* `interp($f, @r)`: interpolate `$f` numbers between every pair of values in `@r`.
* `proj($v1_ref, $v2_ref)`, `orth($v1_ref, $v2_ref)`: vector projection 
* `rdeg`, `drad`: radius to degrees
* `prec`: polar to rectangular
* `rpol`: rectangular to polar
* `entorpy`: Shannon Entorpy in bits
* `haversine`: haversine formula for arc length

### Geographic Functions
The operators in this section refer specifically to the 
`$ ni <data> p'...'`

* `llg` and `ghe`: Latitude and Longitude to geohash
  * `ghe` is a synonym of `llg` provided for backwards compatibility. `llg` should be favored as more explicit.
  * `llg($lat, $lng, $precision)`
    * If `$precision > 0`, returns a geohash with `$precision` base-32 characters of precision. 
    * If `$precision < 0`, returns a geohash with `$precision` (base-2) bits of precision.
* `gll` and `ghd`: geohash to latitude and longitude
  * `ghd` is a synonym of `gll` provided for backwards compatibility. `gll` should be favored as more explicit.
  * `gll($gh_base32)`
     * Returns the corresponding latitude and longitude (in that order) of the center point corresponding to that geohash.
  * `gll($gh_int, $precision)`
    * If the number of bits of precision is specified, `gll` will decode the input integer as a geohash with $precision bits. Returns the  latitude and longitude (in that order) of the southwesternmost point corresponding to that geohash.
* `g3b` and `gb3`: geohash transcoding
  * 
* `ghb`: geohash box
  * `ghb` takes a base-32 geohash and returns the latitude and longitudes corresponding to the north, south, east, and west corners of a box on the earth enclosing this point.
* `lat_lon_dist`: arc distance between points on the earth
  * `lat_lon_dist` computes the distance in kilometers between two points on the earth, accounting for the earth's curvature.
* `gh_dist`: arc distance between two base-32 geohashes
  * Similar to `lat_lon_dist`, this computes the distance along the earth between the centroids of two geohashes.

#### Tagged Geohashes
A tagged geohash is a binary data format that is used to indicate the precision and value of a geohash in a single 8-byte value.

### Time Functions
* `tpe`: time parts to epoch
  * `tpe(@time_pieces)`: Returns the epoch time and assumes that the pieces are year, month, day, hour, minute, and second, in that order.
  * `tpe($time_format, @time_pieces)`: Returns the epoch time, using `$time_format` to determine what the ordered `@time_pieces` are.
* `tep`: time epoch to parts
  * `tep($epoch_time)`: returns the year, month, day, hour, minute, and second in human-readable formatfrom the epoch time.
  * `tep($time_format, $epoch_time)`: returns the specified parts of the date using following `$time_format`.
* `tsec`: Timezone offset in seconds
  * `tep($raw_timestamp + $tsec($lat, $lng))` returns the approximate date and time at the location `$lat, $lng` at a Unix timestamp of `$raw_timestamp`.
* `i2e($iso_8601_time_string)`: ISO-8601 to Epcoh
  * Converts a time in ISO-8601 form to an epoch timestamp.
* `e2i($timestamp, $timezone)`: Epoch to ISO-8601
  * Converts a timestamp in epoch form to ISO-8601
* `ym`: year and month
  * Input: a unix timestamp (seconds since the epoch)
  * Output: the year and month
  * Example: `$ ni i1508348294 p'r ym a'` returns `2017_10`
* `how`: hour of day and weekday
  * Input: a unix timestamp (seconds since the epoch)
  * Output: the day of week and hour of day
  * Example: `$ ni i1508348294 p'r how a'` returns `Wed_17`
* `ttd`, `tth`, `tt15`, `ttm`: Truncate to day, hour, quarter-hour, minute
  * Input: a unix timestamp (seconds since the epoch)
  * Output:
      * `ttd`: the first second of the day that of the argument
      * `tth`: the first second of the hour of the argument
      * `tt15`: the first second of the quarter-hour of the argument 
      * `ttm`: the first second of the minute of the argument
  * Example: `$ ni i1508348294 p'r ttd a, tth a, tt15 a, ttm a'` returns `1508284800	1508346000	1508347800	1508348280`
* `ghl($timestamp, $gh)` and `gh6l($timestamp, $gh60)`: geohash localtime and geohash-60 localtime
  * Input:
  * Output:
  * Example:


### File Utilities

* File Reading
  * `rf`: reads the lines of a file and returns the result as a newline-separated string
  * `rfl`: reads the lines of a file and returns each line as one element of an array.
  * `rfc`: does `rf`, and returns the output after `chomp`ing any trailing newlines.
* File Writing
  * `wf`: write to file
  * `af`: append to file

### Directory Operations
  * `dirbase`: splits the path path `a/b/c` into the base directory name and the final directory or filename. `$ ni ix/y/z p'r dirbase a` returns `x/y	z`.
  * `basename`: splits the last filename or directory off a path. `$ ni ix/y/z p'r dirbase a` returns `x/y`.
  * `dirname`: splits the last filename or directory off a path. `$ ni ix/y/z p'r dirname a` returns `z`.
  * `mkdir_p`: make a directory and all necessary enclosing directories.


### String Utilities
* `startswith($target, $prefix)`, `endswith($target, $suffix)`: returns `1` if the `$target` string starts with (ends with) `$prefix` (`$suffix`).
* `alph($n)`: returns the `$n`th lowercase letter of the alphabet.

### Hash Utilities

* `kbv_dsc(%h)`, `kbv_asc(%h)`: returns the keys of `%h` sorted by the associated value
* `merge_hashes(\%h1, \%h2, \%h3, ...)`, `merge_two_hashes`: merges two hashes using the following recursive strategy:
  * If a key exists in the first hash or the second hash, but not both, add the key and its associated value to the first hash
  * If the key exists in both the first and se
* `sum_hashes`, `sum_two_hashes`: 
* `accumulate_hashes`, `accumulate_two_hashes`
* Hash Constructors
  * `ab_` and others
  * `abS` and others
  * `abSNN` and others
  * `abC` and others
* `p'%h = <col_1><col_2>_ @lines`: Hash constructor
  * Hash constructors are useful for filtering large datasets without having to invoke an expensive sort or an HDFS join. Hash constructors are useful inside of begin blocks, often using the following workflow:
    * Generate a list of things you want to filter, and put it in a data closure. `::ids[list_of_ids]`
    * Convert the data closure to a hash using a begin block (`^{%id_hash = ab_ ids}`)
    * Filter another dataset (`ids_and_data`) using the hash (`exists($id_hash{a()})`)
    * `$ ni ::ids[list_of_ids] ids_and_data rp'^{%id_hash = ab_ ids} exists($id_hash{a()})'`

### JSON Utilities

* Full-Featured but slow
*  `p'json_encode {<row to JSON instructions>}`: JSON Encode
      *  The syntax of the row to JSON instructions is difficult; I believe `ni` will try to interpret value as a `ni` command, but every other unquoted piece of text will be interpreted as 
      *  Here's an example:

```
ni //license FWpF_ p'r pl 3' \
     p'json_encode {type    => 'trigram',
                    context => {w1 => a, w2 => b},
                    word    => c}' \>jsons
```

  * `json_decode`
* Partial-Featured but fast
  * `get` methods 
 	 * `get_scalar($k, $json_str)`
 	 * `get_array($k, $json_str)`
 	 * `get_hash($k, $json_str)`
 * `merge` methods
 	 * `string_merge_hashes($hash_str1, $hash_str2)`: merges two hashes 
 * `delete` methods
   * `delete_scalar($k, $json_str)` 
   * `delete_array($k, $json_str)` 
   * `delete_hash($k, $json_str)` 


### Debugging Utilities
* `dump_data`


## Basic Perl Reducers

The operations here are generally dependent on sorting to function properly, which can make them very expensive to execute on a single machine.

### Streaming Reduce
These operations encapsulate the most common types of reduce operations that you would want to do on a dataset; if your operation is more complicated, it may be more effectively performed using the buffered readahead and line-array reducers.

* `sr`: Reduce over entire stream
  * `$ ni n1E5p'sr {$_[0] + a} 0'`: sum the integers from 1 to 100,000
* `se`: Reduce while equal
  * `@final_state = se {reducer} \&partition_fn, @init_state`
* `sea` through `seq`: Reduce with partition function `a()...q()`
* `rc`: Compound reduce
* `rfn`: Custom compound reduce




## Multiline Reducers
These operations can be used to reduce the data output by the readahead functions. Look at the input provided by the first perl statement, 

* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'sum b_ re {a}'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'sum a_ re {b}'`

`reA` is the more commonly used shorthand for `re {a}`

* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r all {a_($_)} reB'`
* `ni n1p'cart ["a", "a", "b", "c"], [1, 2]' p'r uniq a_ reB'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r maxstr a_ reB'`
* `n1p'cart ["a", "b", "c"], [1, 2]' p'r reduce {$_ + $_[0]} 0, b_ reA'` 

`reB` reduces where both of the first _two_ columns are equal, and `reC` reduces where the first _three_ columns, etc.

## Data Closures in Perl Mappers

Data closures are useful in that they travel with `ni` when `ni` is sent somewhere else, for example over ssh, or as a jar to a Hadoop cluster. Importantly, closures can be accessed from within Perl snippets by using their name.

* `closure_name::[...]`: Create a data closure
  * Any legal `ni` snippet that is executable on the machine from whose context `ni` is being executed.
  * The closure can be referenced within a Perl snippet as  `p' ... closure_name ...'`
* `a_` through `l_`: Multiline Selection Operators
  * Data closures are transferred as an array of lines; in order to access data from a specific column of the data closure, you will need to use multiline operators `a_` through `l_`, which are the multilineanalogs to the line-based operators `a/a()` through `l/l()`.
  * `ni ::data[n1p'cart [1,2], [3,4]'] n1p'a_ data'` works, because `a_` is operating on each element of the array.
  * `ni ::data[n1p'cart [1,2], [3,4]'] n1p'a(data)'` and `ni ::data[n1p'cart [1,2], [3,4]'] n1p'a data'` will raise syntax errors, since `a/a()` are not prepared to deal with the more than one line data in the closure.

## Intermediate Perl Utilities

### Functions

In `ni` you generally should not be defining Perl functions (also called "subroutines").

Perl function definitions are denoted with the keyword `sub`.

Perl functions are often written with a signature that allows them to be written without parentheses. In general, you should use the simplest written form of a function.

### References

* You cannot nest lists or hashes in Perl like you can in Python, and everything that is passed into a Perl function is passed as a flat list;
* Similarly, Perl subroutines also can only return scalars and flat lists. If you want to return multiple lists, or a hash, they must be returned as references.




## Annoyingly Advanced Perl
* `use strict` and the `::` prefix in a Perl Environment
  * When `use strict` is enabled, Perl will complain when you try to create a variable in a Perl snippet that does not start with `::`.
  * The reasons for this are very specific to Perl; if you are a true Perl nerd, you can look them up, but you do not need to know them if you just accept that variables need to start with `::` when you apply `use strict`.
  * It is probably a good idea to `use strict` when the variables you define are sufficiently complex; otherwise you're probably okay not using it.

### Global variables you shouldn't touch
*  `@F`: array of values of the current line. 
 *  Since many other functions depend on `@F`, it is recommended that you use `F_` to access the data indirectly rather than through `@F` itself.
 *  `@F` is re-set every time a fresh line hits the beginning of the Perl mapper.
* `@q`: global pushback queue
  * `@q` persists between fresh lines, and allows past lines to be stored. It is used by `rl` and `pl` to store lines, as well as for the buffered readahead functions `re`, `ru`, `rw`
  * Like `@F`, many functions rely on `@q`; you should not edit or access it directly, and instead use the functions that interact with it.
  * If you want to to store your own array of lines that persists through fresh lines, use a BEGIN block, for example`p'^{@x;} ...; push @x, $data; ...`

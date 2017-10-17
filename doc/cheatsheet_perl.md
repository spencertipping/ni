# `ni` Perl Cheatsheet (alpha release)

`ni` fully supports Perl 5 with backwards compaitibility to 5.08, and most of your `ni` scripts likely should be written in Perl. 

`$ ni data p'<...>'` applies the Perl snippet `<...>` to each row of the stream.



## Basic Perl Syntax

### Sigils

Perl variables are indexed with "sigils" to signify their type. This can be 

* `$x` is a scalar (a number, string, or reference to one of the other types)
* `@x` is an array.
* `$x[0]` is a scalar (its sigil is `$`), whose value is the first element of the array `@x`
* `%h` is a hash.
* `$h{"bar"}` is a scalar (its sigil is `$`), and it is the value of `%h` associated with the key `"bar"`.
* `foo` (without a preceding sigil) is called a "bareword." There are Parsing rules for barewords are If there is a function with no arguments called `foo`, 

### References


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

* `F_` by itself returns an array where the values are the tab-delimited values of the input line.
* Useful for accessing fields beyond the first 12, for example `$ ni <data> F_ 6..15`
* `FM` is the number of fields in the row.
* `FR n` is equivalent to `F_ n..FM`
* `FT n` is equivalent to `F_ 0..(n-1)`


### `rp'...'`: Take rows with Perl

This operator combines the `r` operator from the [operator cheatsheet](cheatsheet_op.md) with Perl, and takes every line that evaluates to true.

* `$ ni <data> rp'<...>'` - take rows where the Perl snippet `<...>` returns a truthy value in Perl. 
* Be careful using `rp'...'` with numeric values, because `0` is falsey in Perl. `$ ni n10 p'r a, 0' rp'b'` returns an empty stream. 

## Perl for `ni`

### Important Perl Builtins

* `lc`
* `uc`
* `substr`
* `split`
* `join`
* `map`
* `grep`
* `keys`
* `values`
* Regular Expressions
  * `$<v> =~ /regex/` -- standard regex form
  * `$<v> =~ s/regex//` -- substitution
  * `$<v> = tr/regex//` -- transliteration (also can be done with `y/regex//`)


### BEGIN and END Blocks

* `p'^{<begin_block>} ...'`: Begin block
  * A begin block is indicated by attaching a caret (`^`) to a block of code (encolsed in `{ }`). Outside of begin blocks, the Perl code is evaluated for every row; inside a begin block, the code is evaluated once and this evaluation occurs before any other code in the mapper is evaluated.
  * Begin blocks are useful for converting data closures to Perl data structures, and defining things like constants and counters that should be maintained over runs with different rows.
* `p'...; END {<end_block>}'`
  * Similar to a begin block, these run only once the last line of the input stream has been processed. These are useful for emitting the value of counters and data structures that have been accumulated over all of the rows of a computation.

### Lexical scoping

Perl is lexically scoped; in general, when using `ni`, you will want to prefix every variable you define within a Perl mapper as scoped to the block, using the keyword `my`, as in `$ ni n10 p'my $x = 0; r $x'`

The exception to this rule is for variables defined within a begin block. In this case `my` is not used at all. See: `$ ni n10 p'^{$x = 0} $x += 1; r $x'`.


## Useful `ni`-specific Perl Subroutines

### Geographic Functions
The operators in this section refer specifically to the 
`$ ni <data> p'...'`

* `ghe`: geohash encoding
  * `ghe($lat, $lng, $precision)`
    * If `$precision > 0`, returns a geohash with `$precision` base-32 characters of precision. 
    * If `$precision < 0`, returns a geohash with `$precision` (base-2) bits of precision.
* `ghd`: geohash decoding
  * `ghd($gh_base32)`
     * Returns the corresponding latitude and longitude (in that order) of the center point corresponding to that geohash.
  * `ghd($gh_int, $precision)`
    * If the number of bits of precision is specified, `ghd` will decode the input integer as a geohash with $precision bits. Returns the  latitude and longitude (in that order) of the southwesternmost point corresponding to that geohash.
* `ghb`: geohash box
  * `ghb` takes a base-32 geohash and returns the latitude and longitudes corresponding to the north, south, east, and west corners of a box on the earth enclosing this point.
* `lat_lon_dist`: arc distance between points on the earth
  * `lat_lon_dist` computes the distance in kilometers between two points on the earth, accounting for the earth's curvature.
* `gh_dist`: arc distance between two base-32 geohashes
  * Similar to `lat_lon_dist`, this computes the distance along the earth between the centroids of two geohashes.

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
  * Converts a timestamp in ISO 8601 form 
* `e2i($timestamp, $timezone)`: Epoch to ISO-8601
  * Converts a timestamp in epoch 
* `ym`: year and month
  * Input: a unix timestamp in 
  * Output: the year and month
  * Example: 
* `how`: hour of day and weekday
  * Input:
  * Output:
  * Example
* `ttd`, `tth`, `tt15`, `ttm`: Truncate to day, hour, quarter-hour, minute
  * Input:
  * Output:
  * Example:
* `ghl($timestamp, $gh)` and `gh6l($timestamp, $gh60)`: geohash localtime and geohash-60 localtime
  * Input:
  * Output:
  * Example:


### List Utlities

* `first(@r)`: first element of `@r` (`== $r[0]`)
* `final(@r)`: last element of `@r` (`== $r[-1]`)
* `rando(@r)`: random element of `@r`
* `max(@r)`, `min(@r)`: numeric `max` and `min`
* `maxstr(@r)`, `minstr(@r)`: lexicographic `max` and `min`
* `take($n, @r)`, `drop($n, @r)`: take or drop the first `$n` elements of `@r` and return the result.
* `take_while(&block, @r)`, `drop_while(@r, &block)`: 
* `take_every(@r)`, `take_even(@r)`, `take_odd(@r)`
* `deltas(@r)`, `totals(@r)`
* `argmax(&block, @r)`, `argmin(&block, @r)`
* `any(&block, @r)`, `all(&block, @r)`
* `uniq(@r)`: unique elements of `@r`
* `freqs`
* `reduce(&block, $start, @r)`
* `reductions(&block, $start, @r)`
* `cart`: Cartesian Product

### File Utilities

* `rf`: 
* `rfl`: 
* `rfc`: 
* `dirbase`: 
* `basename`: 
* `dirname`:
* `mkdir_p`: make a directory and all necessary enclosing directories
* `wf`: write to file
* `af`: append to file

### String Utilities

* `startswith($target, $prefix)`, `endswith($target, $suffix)`:
* `alph($n)`: returns the `$n`-th lowercase letter of the alphabet.

### Hash Utilities

* `kbv_dsc(%h)`, `kbv_asc(%h)`: returns the keys of `%h` sorted by the associated value
* `merge_hashes`, `merge_two_hashes`: 
* `sum_hashes`, `sum_two_hashes`
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
    * `ni ::ids[list_of_ids] ids_and_data rp'^{%id_hash = ab_ ids} exists($id_hash{a()})'`

### JSON Utilities

*  `p'json_encode {<row to JSON instructions>}`: JSON Encode
  *  The syntax of the row to JSON instructions is difficult; I believe `ni` will try to interpret value as a `ni` command, but every other unquoted piece of text will be interpreted as 
  *  Here's an example:
```
ni //license FWpF_ p'r pl 3' \
     p'json_encode {type    => 'trigram',
                    context => {w1 => a, w2 => b},
                    word    => c}' \>jsons
```

* Full-Featured but slow
  * `json_decode`
  * `json_encode`
* Partial-Featured but fast
  * `get_scalar`
  * `get_array`
  * `get_hash`
  * `string_merge_hashes($hash_str1, $hash_str2)`: merges two hashes 


### Debugging Utilities
* `dump_data`

### Line Utilities
* `F_`
* `FM`
* `FR`, `FT`
* `rl`
* `pl`
* `a_` and others: get column from lines
* `a__` and others: get many columns from lines


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


## Buffered Readahead
These operations are good for reducing 

* `rw`: read while
  * `@lines = rw {condition}`: read lines while a condition is met
* `ru`: read until
  * `@lines = ru {condition}`: read lines until a condition is met
* `re`: read equal
  * `@lines = re {condition}`: read lines while the value of the condition is equal.

## Multiline Reducers
These operations can be used to reduce the data output by the readahead functions. Look at the input provided by the first perl statement, 

* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'sum b_ re {a}'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'sum a_ re {b}'`

`rea` is the more commonly used shorthand for `re {a}`

* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r all {a_($_)} reb'`
* `ni n1p'cart ["a", "a", "b", "c"], [1, 2]' p'r uniq a_ reb'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r maxstr a_ reb'`
* `n1p'cart ["a", "b", "c"], [1, 2]' p'r reduce {$_ + $_[0]} 0, b_ rea'` 


## Annoyingly Advanced Perl
* `use strict` and the `::` prefix in a Perl Environment
  * When `use strict` is enabled, Perl will complain when you try to create a variable in a Perl snippet that does not start with `::`.
  * The reasons for this are very specific to Perl; if you are a true Perl nerd, you can look them up, but you do not need to know them if you just accept that variables need to start with `::` when you apply `use strict`.
  * It is probably a good idea to `use strict` when the variables you define are sufficiently complex; otherwise you're probably okay not using it.

## Data Closures in Perl Mappers

Data closures are useful in that they travel with `ni` when `ni` is sent somewhere else, for example over ssh, or as a jar to a Hadoop cluster. Importantly, closures can be accessed from within Perl snippets by using their name.

* `closure_name::[...]`: Create a data closure
  * Any legal `ni` snippet that is executable on the machine from whose context `ni` is being executed.
  * The closure can be referenced within a Perl snippet as  `p' ... closure_name ...'`
* `a_` through `l_`: Multiline Selection Operators
  * Data closures are transferred as an array of lines; in order to access data from a specific column of the data closure, you will need to use multiline operators `a_` through `l_`, which are the multilineanalogs to the line-based operators `a/a()` through `l/l()`.
  * `ni ::data[n1p'cart [1,2], [3,4]'] n1p'a_ data'` works, because `a_` is operating on each element of the array.
  * `ni ::data[n1p'cart [1,2], [3,4]'] n1p'a(data)'` and `ni ::data[n1p'cart [1,2], [3,4]'] n1p'a data'` will raise syntax errors, since `a/a()` are not prepared to deal with the more than one line data in the closure.
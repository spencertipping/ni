#`ni` by Example Chapter 4 (WIP)
Welcome to chapter 4. At this point you have enough skills to read the documentation on your own. As a result, this chapter shoud read a little briefer because it is focused on introducing you to the possibilities of each operator.

##`ni` and bash

[Spencer](https://github.com/spencertipping) refers to `ni` as Huffman-encoded bash, but we haven't given the treatment of `ni` and bash fully yet. If you're already familiar with bash, this will likely be review.

`e` also can use brackets rather than quotes to execute commands. However, this exposes the code that would have been quoted to bash, which might do something you don't want it to.

When you run a script without quotes, bash will first scan for metacharacters. So the script 

```
$ ni e[seq 10 | grep 1]
```

is run as 

```
~/bin/ni "e[seq" "10" | /bin/grep "1]"
```

because bash will execute the pipe first.

The correct way to execute the script: 

```
$ni e'seq 10 | grep 1'
``` 

turns into 
```
exec("/bin/sh", "-c", "stuff")
```

This is a non-obvious feature of the bracketed version of `e`: `e[ word1 word2 ... wordN ]` turns into `exec("word1", "word2", ..., "wordN")`. You'll get shell character expansion with quotes, but not with brackets, the idea being that if you're using brackets, bash has already had a chance to expand the metacharacters like `$foo`.

##`nfu` HDFS Joins

I look forward to the day I rename this section to `ni` HDFS Joins, but for now, the easiest way to do large-scale joins is using `nfu`.

To install:

```
$ git clone git://github.com/spencertipping/nfu
$ cd nfu
$ ln -s $PWD/nfu ~/bin/nfu      ## Or wherever you want to link in your path
```

HDFS joins occur only between the keys of the two datasets (i.e. the first columns of both datasets when expressed as TSV). They will be significantly more efficient when the data are partitioned the same in both the left and right datasets. In order to make sure that this is the case:

```
$ ni :hdfs_path1[ihdfst://<abspath1> HS:_: ]
$ ni :hdfs_path2[ihdfst://<abspath2> HS:_: ]
```

You can appropriately process the checkopoint files to to get the correct paths, then use 

```
nfu hdfs://<abspath1> -j [hdfs://<abspath> 0] _
```




##Cell Operations

Cell operations provide keystroke-efficient ways to do transformations on a single column of the input data. 


####Hashing Algorithms
* `,h`: Murmurhash (deterministic 32-bit hash function)
* `,z`: Intify (hash and then convert hash values to integers starting with 1)
* `,H`: Murmurhash and map the result into the unit interval.

Likely the most important of these functions is the deterministic hashing function, which does a good job of compacting long IDs into 32-bit integers.  This hashing should be good-enough for reasonable-sized data.

Using a little math, with ~40 million IDs, there will be only be about 1% hash collisions, and with 400 million IDs, there will be 10% hash collisions.  See [this](http://math.stackexchange.com/questions/35791/birthday-problem-expected-number-of-collisions) for an explanation.



####Cell Math Operations
* `,e`: Natural exponential e<sup>x</sup>
* `,l`: Natural log (`ln x`)
* `,j<amt>`: Jitter (add uniform random noise in the range `[-amt/2, amt/2]`)
* `,q<amt>`: Round to the nearest integer multiple of `<amt>`


####Column Math Operations
* `,a`: Running average
* `,d`: Difference between consecutive rows
* `,s`: Running sum 


##Intermediate Column Operations
These operations are used to add columns vertically to to a stream, either by merging or with a separate computation.

#### `w`: Append column to stream

`w` adds a column to the end of a stream, up to the minimum length of either stream.

```
$ ni //license w[n3p'a*a']
ni: https://github.com/spencertipping/ni        1
Copyright (c) 2016 Spencer Tipping | MIT license        4
        9
(END)
```

  
####`W`: Prepend column stream

`W` operates like `w`, except its output column is prepended. 

```
$ ni e'echo {a..e}' p'split / /' Wn
1       a
2       b
3       c
4       d
5       e
(END)
```
  

####`v`: Vertical operation on columns

We can upper-case the letters in the previous example via:

```
$ ni e'echo {a..e}' p'split / /' Wn p'r a, uc(b)'
1       A
2       B
3       C
4       D
5       E
(END)
```

However `ni` also offers a shorter syntax using the `v` operator.

```
$ ni e'echo {a..e}' p'split / /' Wn vBpuc
1       A
2       B
3       C
4       D
5       E
(END)
```

**Important Note**: As of 2017-01-22, this operator is too slow to use in production.

Also note that the Perl upper-case operator is written as `puc`, without quotes, as it's good `ni` style to do so.

  
###`j` - streaming join

Streaming joins are performed by matching two sorted streams on the value of their first column.  This significantly limits their utility  because each successfully-joined pair of rows will consume a line from both streams. As such, the `j` operator **DOES NOT** provide a SQL-style join.

There is : 

```
$ ni :letters[e'echo {a..e}' p'split / /'] wn +[letters] wn gABn j[letters]
```

This operation is a little long-winded, and it will probably help to 




####`Y` - dense-to-sparse transformation
`Y` Explodes each row of the stream into several rows, each with three columns:

* The index of the row that the input data that came from
* The index of the column that the input data came from
* The value of the input stream at the row and column specified by the first two columns.


```
$ ni //license FW Y r10
0       0       ni
0       1       https
0       2       github
0       3       com
0       4       spencertipping
0       5       ni
1       0       Copyright
1       1       c
1       2       2016
1       3       Spencer
(END)
```

#### `X` - sparse-to-dense transformation
`X` inverts `Y`: it converts a specifically-formatted 3-column stream into a multiple-column stream. The specification for what the input matrix must look like is described above in the `Y` operator.

```
$ ni //license FW Y r10 X
ni      https   github  com     spencertipping  ni
Copyright       c       2016    Spencer
(END)
```

##Useful `ni`-specific Perl Subroutines
`ni` was developed at [Factual, Inc.](www.factual.com), which works with mobile location data; these operators are general
 
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
  
##Understanding the `ni` monitor
More details [here](monitor.md). Overall:

* Negative numbers = non-rate-determining step
* Positive numbers = rate-determining step
* Large Positive numbers = slow step



##More Perl for `ni`
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

##`m'...'`: Ruby
You have always had permission to use Ruby, but now hopefully you can do it responsibly.

Why am I not giving Ruby the same loving treatment I gave Python in the previous section?

1. The Ruby driver operates in a streaming context, whereas the numpy environment `N` performs operations in-memory. As a result, Python can do things that `ni` alone cannot do, and 
1. That means the primary use of Ruby in `ni` should be for its extensive and customizable libraries (i.e. gems). If you have a Ruby gem that does something that would be odious to implement in Perl, it's a good idea to 


##`l'...'`: Lisp

Lisp is in the same boat as Ruby; it operates in a streaming context, which is much better learned (and in most cases executed) in Perl. One day, when `ni` is a more mature language, and it becomes a multi-quine written in every language that it can also execute.

Lisp ends up even lower on the totem pole than Ruby because it can be a huge pain to install (on Mac, you have to bootstrap installation of SBCL by installing some other Lisp first).

Look, if you're a damn Lisp programmer, you're smart enough to learn Perl. Just do that. I don't know Lisp. Go read these [docs](lisp.md).








  
  
##Plotting with `ni --js`
Check out the [tutorial](tutorial.md) for some examples of cool, interactive `ni` plotting.

**TODO**: Say something useful.


##Custom Compound Reduce
#### `rfn`: Custom compound reduce

**TODO: Understand this**

##Matrix Operations

Operations on huge matrices are not entirely `ni`ic, since they may require space greater than memory, whichwill make them slow. However, operators are provided to improve These operations are suited best to 


* `X<col>`, `Y<col>`, `N<col>`: Matrix Partitioning
  * **TODO**: understand how these actually work.
* `X`: sparse-to-dense transformation
  * In the case that there are collisions for locations `X`, `X` will sum the values
  * For example: `ni n010p'r 0, a%3, 1' X`

##Disk-Backed Data Closures

* `@:[disk_backed_data_closure]`

##Annoyingly Advanced Perl
* `use strict` and the `::` prefix in a Perl Environment
  * When `use strict` is enabled, Perl will complain when you try to create a variable in a Perl snippet that does not start with `::`.
  * The reasons for this are very specific to Perl; if you are a true Perl nerd, you can look them up, but you do not need to know them if you just accept that variables need to start with `::` when you apply `use strict`.
  * It is probably a good idea to `use strict` when the variables you define are sufficiently complex; otherwise you're probably okay not using it.
  
##Binary Operations
In theory, this can save you a lot of space. But I haven't used this in practice.
  

##Less Useful `ni`-specific Perl Extensions


###Array Functions
  * `clip`
  * `within`
  
##Stream Splitting/Joining/Duplication
I don't find these operators particularly useful in practice (with the exception of `=\>` for writing a file in the middle of a long processing pipeline), but it's possible that you will! Then come edit these docs and explain why.

* `+`: append a stream to this one
* `^`: prepend a stream to this one
* `=`: duplicate this stream through a process, discarding its output
   
##Writing Your Own `ni` Extensions
**TODO** Understand how this works

##Obscure Interops/Extensions

* [SQL](sql.md)
* [PySpark](pyspark.md)
* [Scripting](script.md)
* [HTTP Operations](net.md)
* [Defining `ni` functions](fn.md)
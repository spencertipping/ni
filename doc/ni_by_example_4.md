#`ni` by Example Chapter 4 (pre-alpha release)
Welcome to Chapter 4. At this point you have enough skills to read the documentation on your own. As a result, this chapter should read a little briefer because it is focused on introducing you to the possibilities of each operator.

Unlike the other chapters thus far, this chapter has no theme; it's a list of useful operations. This chapter covers some of the interplay between `ni` and `bash`, HDFS joins using `nfu`, The `ni` monitor, cell operations, stream splitting, vertical column operations, sparse matrix operations, Ruby and Lisp operators, ni-specific perl operators, and has another Perl chapter.


##Calling`ni` from other programming languages

You've already been using `ni` within bash, so you've always been using `ni` from another langauge. In this section, we'll make the connections more explicit, and briefly discuss how to use `ni` from Ruby.

####`ni` and bash

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

####`ni` and Ruby
Aside from bash, Ruby feels like the best scripting language from which to call `ni`. Ruby has an easy and simple syntax for calling shell commands using backticks (i.e. `` `cmd` `` will run using the shell). One thing to be aware of is that Ruby backticks will execute using `/bin/sh` and not `/bin/bash`, so to execute your `ni` spells from Ruby, you will want to create them as strings, and execute them with `bash -c "#{cmd}"`

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

##Understanding the `ni` monitor

At this point, you've probably executed a long-running enough `ni` job to see the `ni` monitor appear at the top of your screen. 

* Negative numbers = non-rate-determining step
* Positive numbers = rate-determining step
* Large Positive numbers = slow step

Some things to keep in mind when examining the output of the monitor: if you have access to more compute resources for slow steps, you can use them for that step alone, and this can be done in a very simple way via the `S` horizontal scaling operator.

You may also want to consider refactoring your job to make use of Hadoop Streaming with `HS`, depending on what's suited to your job. More details are available in the monitor [docs](monitor.md) and in the optimization [docs](optimization.md).



##Stream Appending, Interleaving, Duplication, and Buffering
You've seen one of these operators before, the very useful `=\>`, which we've used to write a file in the middle of a stream. The way this works is by duplicating the input stream, and sending one of the duplicated streams silently to an output file. 

####`+` and `^`: Append (Prepend) Stream


`+` and `^` operate sends both streams to ni's output (stdout, usually into a `less` process). Internally, ni is basically doing this:

```
while ($data = read from stream 1) {print $data}
while ($data = read from stream 2) {print $data}
```
The second stream waits to execute until an `EOF` is received from the first stream.


####`%`: Interleave Streams

The bare `%` will interleave streams as the data is output, and will consume both streams fully. For example `$ ni n1E5fAA %[n100]` will put the second stream somewhere in the middle.

You can also call `%#` with a number, in which case the streams will be interleaved with a ratio of `first stream : second stream :: #:1 `

Interleaving streams with a numeric argument will cut off when either stream is exhausted.


####`=`: Duplicate Stream and Discard
This operation is most useful for writing a file in-stream, perhaps with some modifications.

```
$ ni n1E7 =[r5 \>short] r3fAA
1       1
2       2
3       3
(END)
$ ni short
1
2
3
4
5
(END)
```

One thing to be aware of is that the stream's duplication does not block the stream, and that data written to the output file should not be used as input later in the same `ni` spell. 

If you need to do something like this, you may be better off writing two `ni` spells, or using a `ni` [script](script.md).


####`B<op>`: Buffer operation

Likely the most important buffering operation is `Bn`, which buffers data to null. This is especially useful in developing Hadoop Streaming jobs. Hadoop streaming jobs require you to read the entire piece of data in the partfile; this will error out if you try to run `r1000` to get a smaller chunk. 

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

Example:

```
$ ni :letters[e'echo {a..e}' p'split / /'] gA- wn gA +[letters] wn gABn j[letters]
a       5       1       a
b       4       2       b
c       3       3       c
d       2       4       d
e       1       5       e
(END)
```

This operation is a little long-winded, and it will probably help to look at some of the intermediate steps.

```
$ni :letters[e'echo {a..e}' p'split / /'] gA- wn gA
a       5
b       4
c       3
d       2
e       1
```

This is probably a terrible way to generate the letters a through e, joined with then numbers 5 through 1, respectively.

```
$ ni :letters[e'echo {a..e}' p'split / /'] gA- wn gA +[letters]
a       5
b       4
c       3
d       2
e       1
a
b
c
d
e
(END)
```

We've checkpointed our list of letters into a file called `letters`, which allows us to reuse it. The `+` operator appends one stream to another, so at this stage the first half of our stream has numbers appended, and one without.

```
$ ni :letters[e'echo {a..e}' p'split / /'] gA- wn gA +[letters] wn
a       5       1
b       4       2
c       3       3
d       2       4
e       1       5
a       6
b       7
c       8
d       9
e       10
(END)
```

We append another column of numbers; note that `w` adds new columns to every row, but does not make sure that the rows are the same length, so the 


```
$ ni :letters[e'echo {a..e}' p'split / /'] gA- wn gA +[letters] wn gABn
a       5       1
a       6
b       4       2
b       7
c       3       3
c       8
d       2       4
d       9
e       1       5
e       10
(END)
```

We sort the data (necessary to perform the join) first ascending lexicographically by column `A`, and then ascending numerically by column `B`.


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

##Cell Operations

Cell operations are similar to column operations, in that they are keystroke-efficient ways to do transformations on a single column of the input data.


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

These operations are mostly self-explanatory; jitter is often used for `ni --js` operations to create rectangular blocks of color

####Column Math Operations
* `,a`: Running average
* `,d`: Difference between consecutive rows
* `,s`: Running sum 

You can use these `,a` and `,s` to get the average and sum of all data in the stream using, for example:

```
$ ni n1E4 fAA ,aA ,sB r~1
5000.5  50005000
```


##Geographic and Time Perl Functions
`ni` was developed at [Factual, Inc.](www.factual.com), which works with mobile location data; these geographically-oriented operators are open-sourced and highly efficient. There's also a [blog post](https://www.factual.com/blog/how-geohashes-work) if you're interested in learning more.  All of these opertoars work only inside a Perl mapper context (`p'...'`)
 

####`ghe`: geohash encoding
Geohashes are an efficient way of encoding a position on the globe, and is also useful for determining neighboring locations.

The geohashing algorithm works by splitting first on longitude, then by latitude. Thus, geohashes with an odd number of binary bits of precision will be (approximately) squares, and geohashes with an even number of digits will be (approximately) rectangles with their longer side parallel to the equator.

base-32 characters | Approximate geohash size
--- |  ----
1 | 5,000km x 5,000km
2 | 1,250km x 625km
3 | 160km x 160km
4 | 40km x 20km
5 | 5km x 5km
6 | 1.2km x 600m
7 | 150m x 150m
8 | 40m x 20m
9 | 5m x 5m
10 | 1.2m x 60cm
11 | 15cm x 15cm
12 | 4cm x 2cm

`ghe($lat, $lng, $precision)` returns either a geohash in a special base-32 alphabet, or as a long integer.

If `$precision > 0`, the geohash is specified with `$precison` base-32 characters. If `$precision < 0`, it returns an integer geohash with `-$precision` bits of precision.

####`ghd`: geohash decoding

`ni` provides two prototypes for geohash decoding:

`ghd($gh_base32)` Returns the corresponding latitude and longitude (in that order) of the center point corresponding to that geohash.

`ghd($gh_int, $precision)` decodes the input integer as a geohash with `$precision` bits and returns the  latitude and longitude (in that order) of the center point corresponding to that geohash.
    
#### `tpe`: time parts to epoch

`tpe(@time_pieces)`: Returns the epoch time and assumes that the pieces are year, month, day, hour, minute, and second, in that order. You can also specify a format string and call the function as `tpe($time_format, @time_pieces)`.

#### `tep`: time epoch to parts

`tep($epoch_time)`: returns the year, month, day, hour, minute, and second in human-readable format from the epoch time.

A specific format string can also be provided, in which case `tep` is called as `tep($time_format, $epoch_time)`.

#### `timezone_seconds`: convert epoch to local time

`tep($raw_timestamp + $timezone_seconds($lat, $lng))` returns the approximate date and time at the location `$lat, $lng` at a Unix timestamp of `$raw_timestamp`.

For example, let's say you have the Unix timestamp and want to know what time it is at the coordinates: 34.058566<sup>0</sup> N, 118.416526<sup>0</sup> W.

```$ ni i[34.058566 -118.416526] p'my $epoch_time = 1485151713; my $tz_offset = timezone_seconds(a, b); my @local_time_parts = tep($epoch_time + $tz_offset); join "\t", @local_time_parts'
2017    1       22      22      44      33
(END)
```


##More Perl for `ni`
There's a lot more Perl out there to be learned, here's another important salvo, including useful functions, some regex tricks, and the Perl `for` and `map` constructs.

####Useful function list

You've probably come across most of these already, but I'd feel remiss if I didn't put these down somewhere.

* `lc`: lowercase
* `uc`: uppercase
* `substr`: slice a string using a function (you can also slice with a regex)
* `split`: split a string into a list
* `join`: join a list into a string
* `**`: exponent
* `keys %h`: get keys from hash
* `values %h`: get values from hash

#### Regular Expressions
If you need a regex tutorial, [this one](https://regexone.com) looks good to me. Regular expressions are a huge part of Perl, and there are a number of syntaxes for their use of which you should be aware.

######Efficient Regex Design

Regexes should be designed to fail quickly, so reduce the number of possible evaluation paths.  The use of anchor tags (`^$`) and avoiding `.` when possible is also a good idea. 

######String Slicing with Regex
I don't like the of Perl's `substr` method because it's asymmetric. to recover most of what I like about from Python's string slicing syntax, I use regex.

```
$ ni iabcdefgh p'/^(.*).{4}$/'  #[:-4] in Python
abcd
(END)
```

```
$ ni iabcdefgh p'/^.{3}(.*)$/' #[3:] in Python
defgh
(END)
```

```
$ ni iabcdefgh p'/^.*(.{2})$/' #[-2:] in Python
gh
(END)
```

######Using Capture Groups

Capture groups are set off using parentheses; to get them explicitly,  syntactic sugar for the more explicit `my @v = $_ =~ /regex/;`

This will get capture groups and store them in `@v`. Using parentheses around `$v` tells perl to use it in a list context, which will allow you to capture one or more groups.

For example:

```
$ ni iabcdefgh p'my @v = /^(.)(.)/; r @v'
a       b
(END)
```

```
$ ni iabcdefgh p'my ($w) = /^(.)/; r $w'
a
(END)
```

```
$ ni iabcdefgh p'my ($x, $y) = /^(.)(.)/; r $x, $y'
a       b
(END)
```


######Substitution `s///`, Translation `tr///` and `y///`

These operators have a slightly tricky syntax. For example, you can't use these operators the way you'd use capture groups. 

```
$ ni iabcdefgh p'tr/a-z/A-Z/'
8
(END)
```

```
$ ni iabcdefgh p's/abc/ABC/'
1
(END)
```

The reason for these somewhat surp The return value of `tr` and `y` is the number of characters that were translated, and the return value of `s` is 0 if no characters were substituted and ` if characters were.
This also will give somewhat-surprising behavior to code like:

```
$ ni iabcdefgh p'$v = tr/a-z/A-Z/; $v'
8
```

Instead, these operators work as side-effects.

```
$ ni iabcdefgh p'tr/a-z/A-Z/; $_'
ABCDEFGH
(END)
```


```
$ ni iabcdefgh p's/abc/ABC/; $_'
ABCdefgh
(END)
```

####`map`

`map` takes two arguments, a block of code and a perl array, and returns an array.

```
$ ni iabcdefgh p'my @v = map {$_ x 2} split //; r @v'
aa      bb      cc      dd      ee      ff      gg      hh
(END)
```

This code is complicated if you haven't been exposed to the syntax before, so let's break it down.

The block that `map` is using is easy to spot--it's some code wrapped in curly braces, so that's `{$_ x 2}`. Because a `map` block will operate on one argument at a time, we refer to that argument as `$_` (unlike subroutine syntax, which takes its arguments in the array `@_`). The `$_` within the block is **lexically scoped** to the block. Lexical scoping means that the use of `$_` in the block doesn't change its value outside the block. For example:


```
$ ni iabcdefgh p'my @v = map {$_ x 2} split //; r $_'
abcdefgh
(END)
```

Now that we've identified the block, we can identify the array more clearly. The array is `split //`. This looks like a function with no argument. However, when `split` is called with no argument, it will use `$_` by default.  We could have been more explicit and said 

```
$ ni iabcdefgh p'my @v = map {$_ x 2} split //, $_; r $_'
aa      bb      cc      dd      ee      ff      gg      hh
(END)
```

Another facet of the map syntax is that there is no comma between the block and the array. That's not a _nice_ syntax, but Perl isn't nice.  

####`for`
`for` is similar to map, however, it has no return value, thus it is usually used with `r` to pop values out.

####`use strict` and the `::` prefix within `p'...'`

When `use strict` is enabled, Perl will complain when you try to create a variable in a Perl snippet that does not start with `::`.

The reasons for this are very specific to Perl; if you are a true Perl nerd, you can look them up, but you do not need to know them if you just accept that variables need to start with `::` when you apply `use strict`. 

It is probably a good idea to `use strict` when the variables you define are sufficiently complex; otherwise you're probably okay not using it.


##`m'...'`: Ruby
You have always had permission to use Ruby, but I've held off documenting it until you can do so responsibly. Why does Ruby require responsibility, whereas Python/numpy gets a pass (mostly)?

1. The Ruby driver operates in a streaming context, whereas the numpy environment `N` performs operations in-memory. As a result, the Python environment can do things that `ni` alone cannot do. The Ruby operators are weak-tea versions of cutting-edge Perl operators.
1. That means the primary use of Ruby in `ni` should be for its extensive and customizable libraries (i.e. gems). Thus, most of your Ruby code should look something like: `ni ... m'require "lib1"; require "lib2"; <do stuff with those libraries>'`
1. Because the primary use of Ruby is access to Ruby gems, your code becomes less portable. Here's a [good article](http://zachmoshe.com/2015/02/23/use-ruby-gems-with-hadoop-streaming.html) about using Ruby gems with Hadoop Streaming. It's really complicated!
1. Unlike Perl, where text is numbers, the Ruby driver requires that you explicitly cast to a datatype. Just saying, those extra keystrokes add up. Concretely: `ni n4m'r a, a + 1'` will complain about conversion of Fixnum to String without a proper cast.

####`m'a' ... m'q': Ruby Column Accessors

You can get the first 17 tab-delimited columns using the Ruby `a` through `q` operators. However, when using these functions to get numeric values, you must use explicit casts to coerce the value:

* `f`: `to_f`
* `i`: `to_i`
* `s`: `to_s`

To fix the example above, you need to run:

```
$ ni n4m'r a, ai + 1'
1   2
2   3
3   4
4   5
(END)
```

####`m'fields'`: Array of fields.
Analogous to `p'F_ ...'`. `fields` is a Ruby array, so you can use array syntax to get particular fields, for example: 

```
$  ni //license FWr2m'r fields[0..3]'
ni      https   github  com
Copyright       c       2016    Spencer
(END)
```

####`m'r ...'`: Print row
Analogous to `p'r ...'`.



##`l"..."`: Lisp

Lisp is in the same boat as Ruby, as a language that `ni` supports. Lisp operates in a streaming context, which is much better learned (and in most cases executed) in Perl. Lisp ends up even lower on the totem pole than Ruby because it can be a huge pain to install (on Mac, you have to bootstrap installation of SBCL by installing some other Lisp first).

So, if Ruby has gems, why even support Lisp? Here's why:

```
ni n4fAA l"(r (sr ('+ a) ('* b)))"
```

Streaming reduce is ugly in Perl, but smooth and easily understood in Lisp. There's something here, and it's worth working on.

####`l"a" ... l"q": Lisp Column Accessors

Analogous to the Perl and Ruby column accessors.

####`l"(r ...)": Print row

Analogous to `p'r ...'`; prints its arguments.

Look, if you're a damn Lisp programmer, you're smart enough to learn Perl. Just do that. I don't know Lisp. Go read these [docs](lisp.md).
  
  
##Conclusion
You've reached the end of chapter 4 of `ni` by Example, which coincides comfortably with the end of my current understanding of the language. Check back for more chapters, and more old ideas made fresh and useful.
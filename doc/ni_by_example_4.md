#`ni` by Example Chapter 4 (alpha release)
Welcome to Chapter 4. At this point you have enough skills to read the documentation on your own. As a result, this chapter should read a little briefer because it is focused on introducing you to the possibilities of each operator.

Unlike the other chapters thus far, this chapter has no theme; it's a list of useful operations. This chapter covers some of the interplay between `ni` and `bash`, HDFS joins using `nfu`, The `ni` monitor, cell operations, stream splitting, vertical column operations, sparse matrix operations, Ruby and Lisp operators, ni-specific perl operators, and has another Perl chapter.


##Calling`ni` from other programming languages

You've already been using `ni` within bash, so you've always been using `ni` from another langauge. In this section, we'll make the connections more explicit, and briefly discuss how to use `ni` from Ruby.

###`ni` and bash

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
exec("/bin/sh", "-c", "seq 10 | grep 1/")
```

This is a non-obvious feature of the bracketed version of `e`: `e[ word1 word2 ... wordN ]` turns into `exec("word1", "word2", ..., "wordN")`. You'll get shell character expansion with quotes, but not with brackets, the idea being that if you're using brackets, bash has already had a chance to expand the metacharacters like `$foo`.

###`ni` and Ruby
Aside from bash, Ruby feels like the best scripting language from which to call `ni`. Ruby has an easy and simple syntax for calling shell commands using backticks (i.e. `` `cmd` `` will run using the shell). 

Ruby backticks will execute using `/bin/sh` and not `/bin/bash`. There are a few caveats for running in `bin/sh`, which are covered in the [debugging docs](debugging.md).
   


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

You can appropriately process the checkopoint files to to get the correct paths, then use:

```
nfu hdfs://<abspath1> -j [hdfs://<abspath> 0] _
```

`nfu` offers two types of joins, inner and left-outer, and two options for whether the inputs need to be sorted. 

* `i`: Inner join with preceding sort
* `j`: Inner join without sort
* `I`: Left-outer join with preceding sort
* `J`: Left-outer join without sort



##Understanding the `ni` monitor

At this point, you've probably executed a long-running enough `ni` job to see the `ni` monitor appear at the top of your screen. 

* Negative numbers = non-rate-determining step
* Positive numbers = rate-determining step
* Large Positive numbers = slow step

Some things to keep in mind when examining the output of the monitor: if you have access to more compute resources for slow steps, you can use them for that step alone, and this can be done in a very simple way via the `S` horizontal scaling operator.

You may also want to consider refactoring your job to make use of Hadoop Streaming with `HS`, depending on what's suited to your job. More details are available in the [monitor docs](monitor.md) and in the [optimization docs](optimization.md).



##Stream Appending, Interleaving, Duplication, and Buffering
You've seen one of these operators before, the very useful `=\>`, which we've used to write a file in the middle of a stream. The way this works is by duplicating the input stream, and sending one of the duplicated streams silently to an output file. 

###`+` and `^`: Append (Prepend) Stream


Streams in `ni` can be concatenated, however, once concatenated,
any operator on the stream will apply to the stream in its entirety.
`+` and `^` are useful when you have a complicated operator you want
to apply to only part of the stream.

Examples:

```bash
$ ni n3 ^[n05 fAA]
0	0
1	1
2	2
3	3
4	4
1
2
3
```


```bash
$ ni n3 +[n05 fAA]
1
2
3
0	0
1	1
2	2
3	3
4	4
```

`+` and `^` operate sends both streams to ni's output (stdout, usually into a `less` process). Internally, ni is basically doing this:

```
while ($data = read from stream 1) {print $data}
while ($data = read from stream 2) {print $data}
```
The second stream waits to execute until an `EOF` is received from the first stream, so
for even simple streams, you may see a noticeable pause between the end of the
first stream and the start of the second.



###`%`: Interleave Streams

The bare `%` will interleave streams as the data is output, and will consume both streams fully. For example `$ ni nE5fAA %[n100]` will output the second stream non-deterministically; it depends how fast that data is able to stream out.

You can also call `%#` with a positive number, in which case the streams will be interleaved with a ratio of `first stream : second stream :: #:1.` You can also call `%-#`, and the streams will be interleaved with a ratio: `first stream : second stream :: 1:#.`

Interleaving streams with a numeric argument will cut off the output stream when either stream is exhausted.


###`=`: Duplicate Stream and Discard
This operation is most useful for writing a file in-stream, perhaps with some modifications.

```bash
$ ni n10 =[r5 \>short] r3fAA
1	1
2	2
3	3
```

```bash
$ ni short
1
2
3
4
5
```

One thing to be aware of is that the stream's duplication does not block the stream, and that data written to the output file should not be used as input later in the same `ni` spell. 

If you need to do something like this, you may be better off writing two `ni` spells, or using a `ni` [script](script.md).


###`B<op>`: Buffer operation

Likely the most important buffering operation is `Bn`, which buffers data to null. This is especially useful in developing Hadoop Streaming jobs. Hadoop streaming jobs require you to read the entire piece of data in the partfile; this will error out if you try to run `r1000` to get a smaller chunk. 

##Intermediate Column Operations
These operations are used to add columns vertically to to a stream, either by merging or with a separate computation.

### `w`: Append column to stream

`w` adds a column to the end of a stream, up to the minimum length of either stream.

```bash
$ ni //license w[n3p'a*a']
ni: https://github.com/spencertipping/ni	1
Copyright (c) 2016 Spencer Tipping | MIT license	4
	9
```

  
###`W`: Prepend column stream

`W` operates like `w`, except its output column is prepended. 

```bash
$ ni 1p'"a".."e"' p'split / /' Wn
1	a
2	b
3	c
4	d
5	e
```
  

###`v`: Vertical operation on columns

We can upper-case the letters in the previous example via:

```bash
$ ni 1p'"a".."e"' p'split / /' Wn p'r a, uc(b)'
1	A
2	B
3	C
4	D
5	E
```

However `ni` also offers a shorter syntax using the `v` operator.

```bash
$ ni 1p'"a".."e"' p'split / /' Wn vBpuc
1	A
2	B
3	C
4	D
5	E
```

**Important Note**: As of 2017-01-22, this operator is too slow to use in production.

Also note that the Perl upper-case operator is written as `puc`, without quotes, as it's good `ni` style to do so.

  
###`j` - streaming join

**Important note**: [Spencer](https://github.com/spencertipping) considers `j` to be broken. This section is liable to change.

Streaming joins are performed by matching two sorted streams on the value of their first column.  This significantly limits their utility  because each successfully-joined pair of rows will consume a line from both streams. As such, the `j` operator **DOES NOT** provide a SQL-style join.

Example:

```
$ ni 1p'"a".."e"' p'split / /' :letters gA- wn gA +[letters] wn gABn j[letters]
a	5	1	a
b	4	2	b
c	3	3	c
d	2	4	d
e	1	5	e
```

This operation is a little long-winded, and it will probably help to look at some of the intermediate steps.

```bash
$ ni 1p'"a".."e"' p'split / /' :letters gA- wn gA
a	5
b	4
c	3
d	2
e	1
```

This is probably a terrible way to generate the letters a through e, joined with then numbers 5 through 1, respectively.

```bash
$ ni 1p'"a".."e"' p'split / /' :letters gA- wn gA +[letters]
a	5
b	4
c	3
d	2
e	1
a
b
c
d
e
```

We've checkpointed our list of letters into a file called `letters`, which allows us to reuse it. The `+` operator appends one stream to another, so at this stage the first half of our stream has numbers appended, and one without.

```bash
$ ni 1p'"a".."e"' p'split / /' :letters gA- wn gA +[letters] wn
a	5	1
b	4	2
c	3	3
d	2	4
e	1	5
a	6
b	7
c	8
d	9
e	10
```

We append another column of numbers; note that `w` adds new columns to every row, but does not make sure that the rows are the same length, so the 


```bash
$ ni 1p'"a".."e"' p'split / /' :letters gA- wn gA +[letters] wn gABn
a	5	1
a	6
b	4	2
b	7
c	3	3
c	8
d	2	4
d	9
e	1	5
e	10
```

We sort the data (necessary to perform the join) first ascending lexicographically by column `A`, and then ascending numerically by column `B`.



###`Y` - dense-to-sparse transformation
`Y` Explodes each row of the stream into several rows, each with three columns:

* The index of the row that the input data that came from
* The index of the column that the input data came from
* The value of the input stream at the row and column specified by the first two columns.


```bash
$ ni //license FW Y r10
0	0	ni
0	1	https
0	2	github
0	3	com
0	4	spencertipping
0	5	ni
1	0	Copyright
1	1	c
1	2	2016
1	3	Spencer
```

### `X` - sparse-to-dense transformation
`X` inverts `Y`: it converts a specifically-formatted 3-column stream into a multiple-column stream. The specification for what the input matrix must look like is described above in the `Y` operator.

```bash
$ ni //license FW Y r10 X
ni	https	github	com	spencertipping	ni
Copyright	c	2016	Spencer
```

###`Z<n_cols>` - unflatten
`Z` takes data in the form of a single column and returns the same data reshaped into rows with the specified number of columns. Any overhanging data is pushed onto an incomplete row.

```bash
$ ni 1p'"a".."l"' Z4
a	b	c	d
e	f	g	h
i	j	k	l
```

##Cell Operations

Cell operations are similar to column operations, in that they are keystroke-efficient ways to do transformations on a single column of the input data.


###Hashing Algorithms
* `,h`: Murmurhash (deterministic 32-bit hash function)
* `,H`: Murmurhash and map the result into the unit interval.
* `,z`: Intify (hash and then convert hash values to integers starting with 1)

Likely the most important of these functions is the deterministic hashing function, which does a good job of compacting long IDs into 32-bit integers.  This hashing should be good-enough for reasonable-sized data.

Using a little math, with ~40 million IDs, there will be only be about 1% hash collisions, and with 400 million IDs, there will be 10% hash collisions.  See [this](http://math.stackexchange.com/questions/35791/birthday-problem-expected-number-of-collisions) for an explanation.

Let's check that invariant:

```
$ ni n4E7 ,hA Cubuntu[o] uc
39814375
```

That means we had about 200,000 hash collisions in 40 million IDs, a rate of about .5%, which is good enough for my back-of-the envelope calculations.



###Cell Math Operations
* `,e<b>`: exponent b<sup>x</sup> -- deafults to e<sup>x</sup>
* `,l<b>`: logarithm (log<sub>b</sub>x) -- defaults to natural log
* `,j<amt>`: Jitter (add uniform random noise in the range `[-amt/2, amt/2]`)
* `,q<amt>`: Round to the nearest integer multiple of `<amt>`

These operations are mostly self-explanatory; jitter is often used for `ni --js` operations to create rectangular blocks of color.

```sh
# TODO: @bilow I had to disable this test because of floating-point error
$ ni n5 fAAAAAA ,eB ,eC2 ,lD ,lE3 ,eF ,lF
1	2.71828182845905	2	0	0	1
2	7.38905609893065	4	0.693147180559945	0.630929753571457	2
3	20.0855369231877	7.99999999999999	1.09861228866811	1	3
4	54.5981500331442	16	1.38629436111989	1.26185950714291	4
5	148.413159102577	31.9999999999999	1.6094379124341	1.46497352071793	5
```


###Column Math Operations
* `,a`: Running average
* `,d`: Difference between consecutive rows
* `,s`: Running sum 

You can use these `,a` and `,s` to get the average and sum of all data in the stream using, for example:

```bash
$ ni nE4 fAAA ,aA ,sB, ,dC r~1
5000.5	50005000	1
```


##Streaming Reduce
In earlier drafts of `ni` by example, streaming reduce had a much greater role; however, with time I've found that most operations that are done with streaming reduce are more simply performed using buffered readahead; moreover, buffered readahead.  Streaming reduce is still useful and very much worth knowing as an alternative, highly flexible, constant-space set of reduce methods in Perl.

Reduction is dependent on the stream being appropriately sorted, which can make the combined act of sort and reduce expensive to execute on a single machine. Operations like these are (unsurprisingly) good options for using in the combiner or reducer steps of `HS` operations.

These operations encapsulate the most common types of reduce operations that you would want to do on a dataset; if your operation is more complicated, it may be more effectively performed using buffered readahead and multi-line reducers.

### `sr`: Reduce over entire stream

Let's look at a simple example, summing the integers from 1 to 100,000: 

```bash
$ ni n1E5p'sr {$_[0] + a} 0'
5000050000
```

Outside of Perl's trickiness,that the syntax is relatively simple; `sr` takes an anonymous function wrapped in curly braces, and one or more initial values. A more general syntax is the following: 
 
 ```
@final_state = sr {reducer} @init_state
```

To return both the sum of all the integers as well as their product, as well as them all concatenated as a string, we could write the following:

```bash
$ ni n1E1p'r sr {$_[0] + a, $_[1] * a, $_[2] . a} 0, 1, ""'
55	3628800	12345678910
```
A few useful details to note here: The array `@init_state` is read automatically from the comma-separated list of arguments; it does not need to be wrapped in parentheses like one would use to explicitly declare an array in Perl. The results of this streaming reduce come out on separate lines, which is how `p'...'` returns from array-valued functions


### `se`: Reduce while equal
`sr` is useful, but limited in that it will always reduce the entire stream. Often, it is more useful to reduce over a specific set of criteria.

Let's say we want to sum all of the 1-digit numbers, all of the 2-digit numbers, all of the 3-digit numbers, etc. The first thing to check is that our input is sorted, and we're in luck, because when we use the `n` operator to generate numbers for us, they'll come out sorted numerically, which implies they will be sorted by their number of digits.

What we'd like to do is, each time a number streams in, to check if it  number of digits is equal to the number of digits for the previous number we saw in the stream. If it does, we'll add it to a running total, otherwise, we emit the running total to the output stream and start a new running total.

In `ni`-speak, the reduce-while-equal operator looks like this:

```
@final_state = se {reducer} \&partition_fn, @init_state
```

`se` differs from `sr` only in the addition of the somewhat cryptic `\&partition_fn`. This is an anonymous function, which is can be expressed pretty much a block with the perl keyword `sub` in front of it. For our example, we'll use `sub {length}`, which uses the implicit default variable `$_`, as our partition function.

**NOTE**: The comma after `partition_fn` is important.

```bash
$ ni n1000p'se {$_[0] + a} sub {length}, 0'
45
4905
494550
1000
```

That's pretty good, but it's often useful to know what the value of the partition function was for each partition (this is useful, for example, to catch errors where the input was not sorted). This allows us to see how `se` interacts with row-based operators.

```bash
$ ni n1000p'r length a, se {$_[0] + a} sub {length}, 0'
1	45
2	4905
3	494550
4	1000
```

One might worry that the row operators will continue to fire for each line of the input while the streaming reduce operator is running. In fact, the streaming reduce will eat all of the lines, and the first line will only be used once.

If your partition function is sufficiently complicated, you may want to write it once and reference it in multiple places.

```bash
$ ni n1000p'sub len($) {length $_}; r len a, se {$_[0] + a} \&len, 0'
1	45
2	4905
3	494550
4	1000
```

The code above uses a Perl function signature `sub len($)` tells Perl that the function has one argument. The signature for a function with two arguments would be written as `sub foo($$)`. This allows the function to be called as `len a` rather than the more explicit `len(a)`. The above code will work with or without the begin block syntax.


###`sea` through `seq`: Reduce with partition function `a()...q()`

It's very common that you will want to reduce over one of the columns in the data. For example, we could equivalently use the following spell to perform the same sum-over-number-of digits as we did above. 

```bash
$ ni n1000p'r a, length a' p'r b, se {$_[0] + a} \&b, 0'
1	45
2	4905
3	494550
4	1000
```

`ni` offers a shorthand for reducing over a particular column and everything to
its left:

```bash
$ ni n1000p'r length a, a' p'r a, sea {$_[0] + b} 0'
1	45
2	4905
3	494550
4	1000
```


### `rc`: Compound reduce

Consider the following reduction operation, which computes the sum, mean, min and max.

```bash
$ ni n100p'my ($sum, $n, $min, $max) = sr {$_[0] + a, $_[1] + 1,
                                            min($_[2], a), max($_[3], a)}
                                           0, 0, a, a;
            r $sum, $sum / $n, $min, $max'
5050	50.5	1	100
```

For common operations like these, `ni` offers a shorthand:

```bash
$ ni n100p'r rc \&sr, rsum "a", rmean "a", rmin "a", rmax "a"'
5050	50.5	1	100
```


##`ni`-specific Perl Functions
This section collects a number of utility functions that are used within a Perl mapper, but are specific to `ni`.

### I/O Functions

#### `wf`: write to file

* `wf $filename, @lines`: write `@lines` to a file called `$filename`

```
$ ni i[e 1] i[e 2] i[f 1] p'wf a, b_ rea'
e
f
$ ni a
e   1
e   2
$ ni b
f   1
```

#### `af`: append to file

```
```


### Geographic Perl Functions
`ni` was developed at [Factual, Inc.](www.factual.com), which works with mobile location data; these geographically-oriented operators are open-sourced and highly efficient. There's also a [blog post](https://www.factual.com/blog/how-geohashes-work) if you're interested in learning more.  All of these operators work only inside a Perl mapper context (`p'...'`).
 

#### `ghe`: geohash encoding
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

If `$precision > 0`, the geohash is specified with `$precison` base-32 characters. When geohashes are specified in this way, they are referred to in short as `gh<$precision>`, for example gh6, gh8, or gh12. If `$precision < 0`, it returns an integer geohash with `-$precision` bits of precision.

Examples:

```bash
$ ni i[34.058566 -118.416526] p'ghe(a, b, 7)'
9q5cc25
```

```bash
$ ni i[34.058566 -118.416526] p'ghe(a, b, -35)'
10407488581
```

The default is to encode with 12 base-32 characters, i.e. a gh12, or 60 bits of precision.

```bash
$ ni i[34.058566 -118.416526] p'ghe(a, b)'
9q5cc25twby7
```

The parentheses are also often unnecessary, because of the prototypin:

```bash
$ ni i[34.058566 -118.416526] p'ghe a, b, 9'
9q5cc25tw
```

#### `ghd`: geohash decoding

`ni` provides two prototypes for geohash decoding:

`ghd($gh_base32)` Returns the corresponding latitude and longitude (in that order) of the center point corresponding to that geohash.

`ghd($gh_int, $precision)` decodes the input integer as a geohash with `$precision` bits and returns the  latitude and longitude (in that order) of the center point corresponding to that geohash.  As with `ghe`, parentheses are not always necessary.

Examples:

```bash
$ ni i[34.058566 -118.416526] p'r ghd ghe a, b'
34.058565851301	-118.416526280344
```

```bash
$ ni i[34.058566 -118.416526] p'r ghd ghe(a, b, -41), 41'
34.0584754943848	-118.416652679443
```



###Time Perl Functions
    
#### `tpe`: time parts to epoch
`tpe(@time_pieces)`: Returns the epoch time in GMT (see important note below)
and assumes that the pieces are year, month, day, hour, minute, and second, 
in that order. 

```bash
$ ni 1p'tpe(2017, 1, 22, 8, 5, 13)'
1485072313
```

You can also specify a format string and call the function as `tpe($time_format, @time_pieces)`.

```bash
$ ni 1p'tpe("mdYHMS", 1, 22, 2017, 8, 5, 13)'
1485072313
```

**IMPORTANT NOTE**: `tpe` does not work correctly on Mac as of 2017-01-23,
due to an implementation issue with `POSIX::mktime`.
This will result in your code giving the local time rather than GMT; use
`Cubuntu[...]` for the expected behavior.


#### `tep`: time epoch to parts

`tep($epoch_time)`: returns the year, month, day, hour, minute, and second in human-readable format from the epoch time.

```bash
$ ni 1p'r tep tpe 2017, 1, 22, 8, 5, 13'
2017	1	22	8	5	13
```

A specific format string can also be provided, in which case `tep` is called as `tep($time_format, $epoch_time)`.

#### `timezone_seconds`: approximately convert epoch to local time

`tep($raw_timestamp + $timezone_seconds($lat, $lng))` returns the approximate date and time at the location `$lat, $lng` at a Unix timestamp of `$raw_timestamp`.

For example, let's say you have the Unix timestamp and want to know what time it is at the coordinates: 34.058566<sup>0</sup> N, 118.416526<sup>0</sup> W.

```bash
$ ni i[34.058566 -118.416526] p'my $epoch_time = 1485079513; my $tz_offset = timezone_seconds(a, b); my @local_time_parts = tep($epoch_time + $tz_offset); r @local_time_parts'
2017	1	22	2	41	13
```

This correction cuts the globe into 4-minute strips by degree of longitude.
It is meant for approximation of local time, not the actual time.



## More Perl for `ni`
There's a lot more Perl out there to be learned, here's another important salvo, including useful functions, some regex tricks, and the Perl `for` and `map` constructs.

### Useful subroutines

You've probably come across most of these already, but I'd feel remiss if I didn't put these down somewhere.


####String subroutines

* `lc($s)`: lowercase `$s`
* `uc($s)`: uppercase `$s`
* `split(/regex/, $s)`: split a `$s` into a list based on `/regex/`
* `join($sep, @lst)`: join a list into a string, separated by the string `$sep`
* `substr($s, $offset, $length)`: The behavior of `substr` is a little tricky if you have a Python background:
  * If `$offset` is positive, then the start position of the substring will be that position in the string, starting from an index of zero;
  * If `$offset` is negative, then the start position of the substring will be `$offset` charaters from the end of the string.
  * If `$length` is positive, the output substring will take (up to) `$length` characters.
  * If `$length` is negative, the output substring will take characters from `$offset` to the `$length` characters from the end of the string.
 


####Hash subroutines

* `keys %h`: get keys from hash
* `values %h`: get values from hash


####Math subroutines

* `**`: exponent

###Useful variables

####`%ENV`: Hash of Environment Variables

This is way easier to access than in any comparable language. To get the value of the variable you want, remember to dereference with `$ENV{varname}`. Also, it's good to note here that you can more than likely get away with referencing the variable you want using a bareword (unquoted string) rather than a quoted string.

### Regular Expressions
If you need a regex tutorial, [this one](https://regexone.com) looks good to me. Regular expressions are a huge part of Perl, and there are a number of syntaxes for their use of which you should be aware.

####Efficient Regex Design

Regexes should be designed to fail quickly, so reduce the number of possible evaluation paths.  The use of anchor tags (`^$`) and avoiding `.` when possible is also a good idea. 

####String Slicing with Regex
I don't like the of Perl's `substr` method because it's asymmetric. to recover most of what I like about from Python's string slicing syntax, I use regex.

```bash
$ ni iabcdefgh p'/^(.*).{4}$/'  #[:-4] in Python
abcd
```

```bash
$ ni iabcdefgh p'/^.{3}(.*)$/' #[3:] in Python
defgh
```

```bash
$ ni iabcdefgh p'/^.*(.{2})$/' #[-2:] in Python
gh
```

####Using Capture Groups

Capture groups are set off using parentheses; to get them explicitly,  syntactic sugar for the more explicit `my @v = $_ =~ /regex/;`

This will get capture groups and store them in `@v`. Using parentheses around `$v` tells perl to use it in a list context, which will allow you to capture one or more groups.

For example:

```bash
$ ni iabcdefgh p'my @v = /^(.)(.)/; r @v'
a	b
```

```bash
$ ni iabcdefgh p'my ($w) = /^(.)/; r $w'
a
```

```bash
$ ni iabcdefgh p'my ($x, $y) = /^(.)(.)/; r $x, $y'
a	b
```


####Substitution `s///`, Transliteration `tr///` and `y///`

These operators have a slightly tricky syntax. For example, you can't use these operators the way you'd use capture groups. 

```bash
$ ni iabcdefgh p'tr/a-z/A-Z/'
8
```

```bash
$ ni iabcdefgh p's/abc/ABC/'
1
```

The reason for these somewhat surp The return value of `tr` and `y` is the number of characters that were translated, and the return value of `s` is 0 if no characters were substituted and ` if characters were.
This also will give somewhat-surprising behavior to code like:

```bash
$ ni iabcdefgh p'$v = tr/a-z/A-Z/; $v'
8
```

Instead, these operators work as side-effects.

```bash
$ ni iabcdefgh p'tr/a-z/A-Z/; $_'
ABCDEFGH
```

```bash
$ ni iabcdefgh p's/abc/ABC/; $_'
ABCdefgh
```

However, as you might expect from Perl, there is a syntax that allows `s` to return the value; however, this will not work on Perls before 5.12 or 5.14.


```sh
$ ni iabcdefgh p's/abc/ABC/r'
ABCdefgh
```



###`use strict` and the `::` prefix within `p'...'`

When `use strict` is enabled, Perl will complain when you try to create a variable in a Perl snippet that does not start with `::`.

The reasons for this are very specific to Perl; if you are a true Perl nerd, you can look them up, but you do not need to know them if you just accept that variables need to start with `::` when you apply `use strict`. 

It is probably a good idea to `use strict` when the variables you define are sufficiently complex; otherwise you're probably okay not using it.


##`m'...'`: Ruby
You have always had permission to use Ruby, but I've held off documenting it until you can do so responsibly. Why does Ruby require responsibility, whereas Python/numpy gets a pass (mostly)?

1. The Ruby driver operates in a streaming context, whereas the numpy environment `N` performs operations in-memory. As a result, the Python environment can do things that `ni` alone cannot do. The Ruby operators are weak-tea versions of cutting-edge Perl operators.
1. That means the primary use of Ruby in `ni` should be for its extensive and customizable libraries (i.e. gems). Thus, most of your Ruby code should look something like: `ni ... m'require "lib1"; require "lib2"; <do stuff with those libraries>'`
1. Because the primary use of Ruby is access to Ruby gems, your code becomes less portable. Here's a [good article](http://zachmoshe.com/2015/02/23/use-ruby-gems-with-hadoop-streaming.html) about using Ruby gems with Hadoop Streaming. It's really complicated!
1. Unlike Perl, where text is numbers, the Ruby driver requires that you explicitly cast to a datatype. Just saying, those extra keystrokes add up. Concretely: `ni n4m'r a, a + 1'` will complain about conversion of Fixnum to String without a proper cast.

###`m'a' ... m'q'`: Ruby Column Accessors

You can get the first 17 tab-delimited columns using the Ruby `a` through `q` operators. However, when using these functions to get numeric values, you must use explicit casts to coerce the value:

* `f`: `to_f`
* `i`: `to_i`
* `s`: `to_s`

To fix the example above, you need to run:

```bash
$ ni n4m'r a, ai + 1'
1	2
2	3
3	4
4	5
```

###`m'fields'`: Array of fields.
Analogous to `p'F_ ...'`. `fields` is a Ruby array, so you can use array syntax to get particular fields, for example: 

```bash
$ ni //license FWr2m'r fields[0..3]'
ni	https	github	com
Copyright	c	2016	Spencer
```

###`m'r ...'`: Print row
Analogous to `p'r ...'`.



##`l"..."`: Lisp

Lisp is in the same boat as Ruby, as a language that `ni` supports. Lisp operates in a streaming context, which is much better learned (and in most cases executed) in Perl. Lisp ends up even lower on the totem pole than Ruby because it can be a huge pain to install (on Mac, you have to bootstrap installation of SBCL by installing some other Lisp first).

So, if Ruby has gems, why even support Lisp? Here's why:

```bash
$ ni n4fAA l"(r (sr ('+ a) ('* b)))"
10	24
```

Streaming reduce is ugly in Perl, but smooth and easily understood in Lisp. There's something here, and it's worth working on.

###`l"a" ... l"q"`: Lisp Column Accessors

Analogous to the Perl and Ruby column accessors.

###`l"(r ...)"`: Print row

Analogous to `p'r ...'`; prints its arguments.

Look, if you're a damn Lisp programmer, you're smart enough to learn Perl. Just do that. I don't know Lisp. Go read these [docs](lisp.md).
  
  
##Conclusion
You've reached the end of chapter 4 of `ni` by Example, which coincides comfortably with the end of my current understanding of the language. Check back for more chapters to come, and more old ideas made fresh, useful, and fast.

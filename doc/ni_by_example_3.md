# `ni` by Example, Chapter 3 (beta release)

## Introduction

In this section, we introduce `ni`-specific Perl extensions. 
`ni` was developed at [Factual, Inc.](www.factual.com), which works with mobile location data; these geographically-oriented operators are open-sourced and highly efficient. There's also a [blog post](https://www.factual.com/blog/how-geohashes-work) if you're interested in learning more.

The next set of operators work with multiple lines of input data; this allows reductions over multiple lines to take place. This section ends with a discussion of more I/O functions.


## `ni`-specific Perl Functions

### Geographic Perl Functions

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

The parentheses are also often unnecessary, because of the prototyping:

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

#### `ghb`: geohash bounding box

For plotting, it is useful to get the latitude and longitude coordinates of  the box that is mapped to a particular geohash. `ghb` returns the coordinates of that box in order: northernmost point, southernmost point, easternmost point, westernmost point.

```bash
$ ni 1p'r ghb "95qc"'
18.6328123323619	18.45703125	-125.156250335276	-125.5078125
```

#### `gh_dist`: distance between geohash centroids
It is also useful to compute the distance between the center points of geohashes; this is implemented through `gh_dist`.

```bash
$ ni 1p'gh_dist "95qcc25y", "95qccdnv", mi'
1.23981551084308
```

When the units are not specified, `gh_dist` gives its answers in kilometers, which can be specified explicitly as "km". Other options include feet ("ft") and meters ("m"). To specify meters, you will need to use quotation marks, as the bareword `m` is taken.

```bash
$ ni 1p'gh_dist "95qcc25y", "95qccdnv"'
1.99516661267524
```


#### `lat_lon_dist`: distance between points on the globe
This subroutine implements the haversine distance formula. Like `gh_dist`, it also takes units ("mi", "km", "ft", "m"), and will return the distance in kilometers if no other information is given.

```bash
$ ni 1p'lat_lon_dist 31.21984, 121.41619, 34.058686, -118.416762'
10426.7380460312
```


### Time Perl Functions

`tpe` and `tep` are the most flexible and powerful time functions in `ni`'s arsenal for converting between Unix timestamps and date. Other important functions here are used for localization by lat/long or geohash, and several syntactic sugars are provided for common mathematical and semntic time operations.

    
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

#### `tep`: time epoch to parts

`tep($epoch_time)`: returns the year, month, day, hour, minute, and second in human-readable format from the epoch time.

```bash
$ ni 1p'r tep tpe 2017, 1, 22, 8, 5, 13'
2017	1	22	8	5	13
```

A specific format string can also be provided, in which case `tep` is called as `tep($time_format, $epoch_time)`.

#### `tsec`: approximately convert epoch to local time

`tep($raw_timestamp + tsec($lat, $lng))` returns the approximate date and time at the location `$lat, $lng` at a Unix timestamp of `$raw_timestamp`.

For example, let's say you have the Unix timestamp and want to know what time it is at the coordinates: 34.058566<sup>0</sup> N, 118.416526<sup>0</sup> W.

```bash
$ ni i[34.058566 -118.416526] \
     p'my $epoch_time = 1485079513; my $tz_offset = tsec(a, b); 
       my @local_time_parts = tep($epoch_time + $tz_offset); 
       r @local_time_parts'
2017	1	22	2	41	13
```

This correction cuts the globe into 4-minute strips by degree of longitude.
It is meant for approximation of local time, not the actual time, which depends much more on politics and introduces many more factors to think about.

#### `ghl` and `gh6l`: approximate conversion to local time from geohash/gh60 

`ghl` and `gh6l` get local time from an input geohash input in base-32 (`ghl`) or base-10 representation of a the geohash-60 in binary (`gh6l`).

```bash
$ ni i[34.058566 -118.416526] p'ghe a, b' \
     p'my $epoch_time = 1485079513; 
       my @local_time_parts = tep ghl($epoch_time, a); 
       r @local_time_parts'
2017	1	22	2	41	13
```

```bash
$ ni i[34.058566 -118.416526] p'ghe a, b, -60' \
     p'my $epoch_time = 1485079513; 
       my @local_time_parts = tep gh6l($epoch_time, a); 
       r @local_time_parts'
2017	1	22	2	41	13
```


#### `i2e` and `e2i`: ISO 8601 time <=> epoch time

ISO 8601 is a standardized time format defined by the International Organization for Standards. More information on formats is available [here](https://en.wikipedia.org/wiki/ISO_8601). `ni` implements decoding for date-time-timezone formatted ISO data into Unix timestamps. 

A similar non-ISO format (allowing for a space between date and time, and decimal precision to the time, which is ignored), is also supported.

```bash
$ ni i2017-06-24T18:23:47+00:00 i2017-06-24T19:23:47+01:00 \
     i2017-06-24T15:23:47-03:00 i2017-06-24T13:08:47-05:15 \
     i20170624T152347-0300 i20170624T182347Z \
     i2017-06-24T18:23:47+00:00 i"2017-06-24 18:23:47.683" p'i2e a'
1498328627
1498328627
1498328627
1498328627
1498328627
1498328627
1498328627
1498328627
```

`ni` can also format epoch timestamps in an ISO 8601-compatible form. To do this, input an epoch timestamp and either a floating number of hours, or a timezone format string, for example `"+10:00"`.

```bash
$ ni i2017-06-24T18:23:47+00:00 p'i2e a' \
     p'r e2i a; r e2i a, -1.5; r e2i a, "+3";
       r e2i a, "-05:45"'
2017-06-24T18:23:47Z
2017-06-24T16:53:47-01:30
2017-06-24T21:23:47+03:00
2017-06-24T12:38:47-05:45
```

These all represent the same instant:

```bash
$ ni i2017-06-24T18:23:47+00:00 p'i2e a' \
     p'r e2i a; r e2i a, -1.5; r e2i a, "+3";
       r e2i a, "-05:45"' p'i2e a'
1498328627
1498328627
1498328627
1498328627
```

#### `tpi`: time parts to ISO 8601
Takes 7 mandatory arguments: year, month, day, hour, minute, second, and time zone. The time zone can either be reported as a string `-08:00` or a number `-8`.

```bash
$ ni 1p'r tpi tep(tpe(2018, 1, 14, 9, 12, 31)), "Z"'
2018-01-14T09:12:31Z
```

To verify correctness:

```bash
$ ni 1p'r i2e tpi tep(1515801233), "Z"'
1515801233
```


#### `dow`, `hod`, `how`, `ym`: Day-of-Week, Hour-of-Day, Hour-of-Week, Year-and-Month shorthands

These functions give the 3-letter abbreviation for day of week, hour of day, and hour of week, and year + month.

```bash
$ ni i[34.058566 -118.416526] p'ghe a, b, -60' \
     p'my $epoch_time = 1485079513; dow gh6l($epoch_time, a)'
Sun
```

```bash
$ ni i[34.058566 -118.416526] p'ghe a, b, -60' \
     p'my $epoch_time = 1485079513; hod gh6l($epoch_time, a)'
2
```

```bash
$ ni i[34.058566 -118.416526] p'ghe a, b, -60' \
     p'my $epoch_time = 1485079513; how gh6l($epoch_time, a)'
Sun_02
```

```bash
$ ni i[34.058566 -118.416526] p'ghe a, b, -60' \
     p'my $epoch_time = 1485079513; ym gh6l($epoch_time, a)'
2017-01
```

#### `ttd`, `tth`, `tt15`, `ttm`: truncate to day, hour, quarter-hour, and minute

These functions truncate dates, which is useful for bucketing times; they're much faster than calls to the POSIX library, which can make them more practical in performance-environments (`HS`, for example).

```bash
$ ni i1494110651 p'r tep ttd(a); r tep tth(a);
                   r tep tt15(a); r tep ttm(a); r tep a'
2017	5	6	0	0	0
2017	5	6	22	0	0
2017	5	6	22	30	0
2017	5	6	22	44	0
2017	5	6	22	44	11
```


## Buffered Readahead and Multiline Selection

So far, we have only seen many ways to operate on data, but only one way to reduce it, the counting operator `c`. Buffered readahead allows us to perform operations on many lines at onceThese operations are used to convert columnar data into arrays of lines.


### `rl`, `rw`, `ru`, `re`: Buffered Readahead

In general, the types of reductions that can be done with buffered readahead and multiline reducers can also be done with streaming reduce (discussed in a later chapter); however, the syntax for  buffered readahead is often much simpler. Generating arrays of lines using readahead operations is a common motif in `ni` scripts:

* `rl(n)`: read `n` lines
  * `@lines = rl(n)`: `n` is optional, and if it is not given, only one line will be read.
* `rw`: read while
  * `@lines = rw {condition}`: read lines while a condition is met
* `ru`: read until
  * `@lines = ru {condition}`: read lines until a condition is met
* `re`: read equal
  * `@lines = re {condition}`: read lines while the value of the condition is equal.


```
$ ni n10 p'r rl 3'
1	2	3
4	5	6
7	8	9
10
```

```bash
$ ni n10p'r rw {a < 7}'
1	2	3	4	5	6
7
8
9
10
```

```bash
$ ni n10p'r ru {a % 4 == 0}'
1	2	3
4	5	6	7
8	9	10
```

```bash
$ ni n10p'r re {int(a**2/30)}'
1	2	3	4	5
6	7
8	9
10
```


### `a_` through `l_`: Multiline Selection operations 
You have now seen two ways to generate arrays of lines: buffered readahead and data closures. In order to access data from a specific column of a line array, you will need to use multiline operators `a_` through `l_`, which are the multiline analogs to the line-based operators `a/a()` through `l/l()`.


```bash
$ ni i[j can] i[j you] i[j feel] \
     i[k the] i[k love] i[l tonight] \
     p'my @lines = re {a}; r @lines;'
j	can	j	you	j	feel
k	the	k	love
l	tonight
```

`re {a}` will give us all of the lines where `a()` has the same value (i.e. the first column is the same. What gets stored in the variable`my @lines` is the array of text lines where `a()` has the same value: `("j\tcan", "j\tyou", "j\tfeel")`. 

To get data from one particular column of this data, we can use the multiline selectors; let's say we only wanted the second column of that data--we can get that by applying `b_` to `@lines`.

```bash
$ ni i[j can] i[j you] i[j feel] \
     i[k the] i[k love] i[l tonight] \
     p'my @lines = re {a}; r b_(@lines)'
can	you	feel
the	love
tonight
```

That's given us the second element of each line that started with the 

As with most everything in `ni`, we can shorten this up considerably; we can drop the parentheses around `@lines`, and use `rea` as a shorthand for `re {a}`

```bash
$ ni i[j can] i[j you] i[j feel] \
     i[k the] i[k love] i[l tonight] \
     p'my @lines = rea; r b_ @lines'
can	you	feel
the	love
tonight
```

And if we want to do this in a single line, we can combine multiline selection with buffered readahed:

```bash
$ ni i[j can] i[j you] i[j feel] \
     i[k the] i[k love] i[l tonight] \
     p'r b_ rea'
can	you	feel
the	love
tonight
```

The last two examples are used commonly. The former, more explicit one is often used when we want more than one reduction:

```bash
$ ni i[j can] i[j you] i[j feel] \
     i[k the] i[k love] i[l tonight] \
     p'my @lines = rea; r b_ @lines; r a_ @lines'
can	you	feel
j	j	j
the	love
k	k
tonight
l
```

Another common motif is getting the value of 

### `reb ... rel`: Reduce while multiple columns are equal

In the previous section, we covered `rea` as syntactic sugar for `re {a}`

```bash
$ ni i[a x first] i[a x second] \
     i[a y third] i[b y fourth] p'r c_ rea'
first	second	third
fourth
```

This syntactic sugar is slightly different for `reb, rec, ..., rel`.

Let's take a look at what happens with `re {b}`:

```bash
$ ni i[a x first] i[a x second] \
     i[a y third] i[b y fourth] p'r c_ re {b}'
first	second
third	fourth
```

In general, that's not quite what we want; when we do reduction like this, we've probably already sorted our data appropriately, so we'll want to reduce over *all* of the columns; writing a function that reduces over columns `a` and `b` is a little clunky, so we use `reb` as a syntactic sugar for that function.

```bash
$ ni i[a x first] i[a x second] \
     i[a y third] i[b y fourth] p'r c_ reb'
first	second
third
fourth
```

This is our desired output.


### Caveats

* `$ ni ::data[n5] 1p'a(data)'` and `$ ni ::data[n5] 1p'a data'` will raise syntax errors, since `a/a()` are not prepared to deal with the more than one line data in the closure.
* `$ ni ::data[n5] 1p'a_ data'` works, because `a_` operates on each line.

```bash
$ ni ::data[n5] 1p'a_ data'
1
2
3
4
5
```


### `a__` through `l__`: Select-to-end-of-line
These are useful for collecting data with an unknown shape.

```bash
$ ni i[m 1 x] i[m 2 y s t] \
     i[m 3 yo] i[n 5 who] i[n 6 let the dogs] p'r b__ rea'
1	x	2	y	s	t	3	yo
5	who	6	let	the	dogs
```

In particular, these operations can be used in conjunction with Hadoop streaming to join together data that have been computed separately.


## `ni`-specific Array Processors

These functions pair well with the multiline reducers introduced in the previous section.


### `min`, `max`, `minstr`, `maxstr`

Recall that text is numbers (and numbers are text) in Perl. Thus, there are two sets of methods for finding the minimum and maximum, depending on whether you want it in a numeric context (`min`, `max`) or in a string context (`minstr`, `maxstr`)


```bash
$ ni i[1 2 3] p'r min F_; r max F_'
1
3
```

```bash
$ ni i[c a b] p'r minstr F_; r maxstr F_'
a
c
```

Be careful using these functions with both numeric and string arguments in the same line:

```sh
$ ni i[1 2 3 a b c] p'r min F_; r max F_; r minstr F_; r maxstr F_'
b
3
1
c
```

### `sum`, `prod`, `mean`
These functions provide the sum, product, and average of a numeric array:

```bash
$ ni i[2 3 4] p'r sum(F_), prod(F_), mean(F_)'
9	24	3
```

### `uniq`, `freqs`

`uniq` returns an array of the unique elements of an array.

```bash
$ ni i[a c b c c a] p'my @uniqs = uniq F_; r sort @uniqs'
a	b	c
```

`freqs` returns a reference to a hash that contains the count of each unique element in the input array.

```bash
$ ni i[a c b c c a] p'my %h = %{freqs F_}; 
                      r($_, $h{$_}) for sort keys %h'
a	2
b	1
c	3
```


### `any`, `all`, `argmax`, `argmin`
These functions take two arguments: the first is a code block or a function, and the second is an array. 

`any` and `all` return the logical OR and logical AND, respectively of the code block mapped over the array.


```bash
$ ni i[2 3 4] p'r any {$_ > 3} F_; r all {$_ > 3} F_'
1
0
```


`argmax` returns the value in the array that achieves the maximum of the block passed in, and `argmin` returns the value in the array that achieves the minimum of the function. 


```bash
$ ni i[aa bbb c] p'r argmax {length} F_; r argmin {length} F_'
bbb
c
```

If there are any ties, the first element is picked.

```bash
$ ni i[aa bbb c ddd e] p'r argmax {length} F_; r argmin {length} F_'
bbb
c
```



### `cart`: Cartesian Product
To generate examples for our buffered readahead, we'll take a short detour the builtin `ni` operation `cart`.

```bash
$ ni 1p'cart [10, 20], [1, 2, 3]'
10	1
10	2
10	3
20	1
20	2
20	3
```

The output of `cart` will have the first column varying the least, the second column varying the second least, etc. so that the value of the last column will change for every row if its values are all distinct.


Note that `cart` takeas array references (in square brackets), and returns array references. `ni` will interpret these array references as rows, and expand them. Thus `r`, when applied to `cart`, will likely not produce your desired results.

```sh
$ ni 1p'r cart [1], ["a", "b", "c"]'
ARRAY(0x7ff2bb109568)   ARRAY(0x7ff2ba8c93c8)   ARRAY(0x7ff2bb109b80)
```

### Functional Programming Basics: `take`, `drop`, `take_while`, `drop_while`


`take` and `drop` get (or get rid of) the specified number of elements from an array.

```bash
$ ni i[1 2 3 4 5 6 7] p'r take 3, F_; r drop 3, F_'
1	2	3
4	5	6	7
```

`take_while` and `drop_while` take an additional block; they `take` or `drop` while the function is true.

```bash
$ ni i[1 2 3 4 5 6 7] p'r take_while {$_ < 3} F_;
                        r drop_while {$_ < 3} F_'
1	2
3	4	5	6	7
```

Some day, `ni` will get lazy sequences, and this will be useful. Until that day...


## Hash Constructors

### `p'%h = <key_col><val_col>_ @lines'`: Hash constructor

Hash constructors are useful for filtering or adding data. There are many hash constructor functions all with the same syntax. For example, `my %h = ab_ @lines` returns a hash where the keys are taken from column `a` and the values are taken from column `b`. `my %h = fd_ @lines` will return a hash where 


```bash
$ ni i[a 1] i[b 2] i[foo bar] \
     p'my @lines = rw {1};
       my %h = ab_ @lines; my @sorted_keys = sort keys %h;
       r @sorted_keys; r map {$h{$_}} @sorted_keys'
a	b	foo
1	2	bar
```

There's a nice piece of Perl syntactic sugar for the last return statement that gets all of the values out of a hash associated with the elements of an array:

```bash
$ ni i[a 1] i[b 2] i[foo bar] \
     p'my @lines = rw {1};
       my %h = ab_ @lines; my @sorted_keys = sort keys %h;
       r @sorted_keys; r @h{@sorted_keys}'
a	b	foo
1	2	bar
```

Hashes are often combined with closures to do filtering operations:

```bash
$ ni ::passwords[idragon i12345] i[try this] i[123 fails] \
     i[dragon does work] i[12345 also works] \
     i[other ones] i[also fail] \
     rp'^{%h = ab_ passwords} exists($h{+a})'
dragon	does	work
12345	also	works
```

#### Caveat

Constructing hashes from closures can get very large; if you have more than 1 million keys in the hash, the hash will likely grow to several gigabytes in size. If you need to use a hash with millions of keys, you'll often be better off using a Bloom filter, described in Chapter 6. 


### `p'%h = <key_col><val_col>S @lines'`: Accumulator hash constructor

This is useful for doing reduction on data you've already reduced; for example, you've counted the number of neighborhoods in each city in each country and now want to count the number of neighborhoods in each country.

```bash 
$ ni i[x k 3] i[x j 2] i[y m 4] i[y p 8] i[y n 1] p'r acS rea'
x	5
y	13
```

If you have ragged data, where a value may not exist for a particular column, a convience method `SNN` gives you the non-null values. See the next section for the convenience methods `kbv_dsc`.

```bash 
$ ni i[y m 4 foo] i[y p 8] i[y n 1 bar] \
     p'%h = dcSNN rea; 
       @sorted_keys = kbv_dsc %h;
       r($_, $h{$_}) for @sorted_keys'
foo	4
bar	1
```


### `p'kbv_asc %h'` and `p'kbv_dsc %h'`: Sort hash keys by value

This is syntactic sugar for Perl's sort function applied to keys of a hash.

```bash 
$ ni i[x k 3] i[x j 2] i[y m 4] i[y p 8] i[y n 1] i[z u 0] \
     p'r acS rea' p'r kbv_dsc(ab_ rl(3))'
y	x	z
```


```bash 
$ ni i[x k 3] i[x j 2] i[y m 4] i[y p 8] i[y n 1] i[z u 0] \
     p'r acS rea' p'r kbv_asc(ab_ rl(3))'
z	x	y
```

## I/O Perl Functions

### `wf`: write to file

`wf $filename, @lines`: write `@lines` to a file called `$filename`

```sh
$ ni i[file1 1] i[file1 2] i[file2 yo] p'wf a, b_ rea'
file1
file2
$ ni file1
1
2
$ ni file2
yo
```

### `af`: append to file

```sh
$ ni i[file3 1] i[file3 2] i[file4 yo] i[file3 hi] p'af a, b_ rea'
file3
file4
file3
$ ni file3
1
2
hi
$ ni file4
yo
```

`af` is not highly performant; in general, if you have to write many lines to a file, you should process and sort the data in such a way that all lines can be written to the same file at once with `wf`. `wf` will blow your files away though, so be careful.


## JSON I/O

We'll spend the rest of this chapter discussing JSON and directory I/O; the former is fundamental to a lot of the types of operations that `ni` is good at, the latter will expand your understanding of `ni`'s internal workings.

### `D:<field1>,:<field2>...`: JSON Destructure

`ni` implements a very fast JSON parser that is great at pulling out string and numeric fields.  As of this writing (2017-07-12), the JSON destructurer does not support list-based fields in JSON.

### `p'json_encode <hash or array reference>`: JSON Encode

The syntax of the row to JSON instructions is similar to hash construction syntax in Ruby. 
  


### Other JSON parsing methods
Some aspects of JSON parsing are not quite there yet, so you may want to use (also very fast) raw Perl to destructure your JSONs.


## `ni` Philosophy and Style

If you've made it this far in the tutorial, you now have enough tools to be extremely productive in `ni`. If you're ready to get off the crazy ride of this tutorial and get to work, here's a great point to stop. Before you go, though, it will help to take a few minutes to think through `ni`'s philosophy and style, and how those two intersect.

### `ni` optimizes at-will programmer productivity
Many data science projects start with a request that you have never seen before; in this case, it's often easier to start from very little and be able to build rapidly, than it is to have a huge personal library of Python scripts and Jupyter notebooks that you can bend to fit the task at hand.

`ni` is great at generating throwaway code, and this code can be made production-ready through `ni` scripting, discussed in the next chapter.

### Mastery counts

Just because you can read a Python script and understand what it does at a basic level does not mean you can code in Python, and Python can trick very intelligent people into thinking they know what they're doing even when they don't. The opposite is true of `ni`. It will be inherently obvious when you don't know something in `ni`, because if you don't know something, it'll be likely that the part of the spell you're reading won't make sense.

This is a gruff approach to programming, but it's not unfriendly. `ni` doesn't allow you to just get by--your only option is mastering `ni` one piece at a time.

### Outsource hard jobs to more appropriate tools

`ni` is a domain-specific language; its domain is processing single lines and chunks of data that fit in memory

* Because of this philosophy, `ni` is fantastic for data munging and cleaning.
* Because of this philosophy, large-scale sorting is not a `ni`-ic operation, while gzip compression is.
* Because of this philosophy, `ni` relies heavily on Hadoop for big data processing. Without Hadoop, most sufficiently complicated operations become infeasible from a runtime perspective once the amount of data exceeds a couple of gigabytes uncompressed.

Some jobs that are difficult for `ni`, and some ways to resolve them:

* Sorting
  * Challenge: Requires buffering of entire stream (possibly to disk, which is slow)
  * Solution: Hadoop Streaming will do much of the heavy lifting for you in any sort, and will distribute the computation so it's easy.
* Matrix Multiplication
  * Challenge: Difficult to implement in a streaming context
  * Solution: Numpy operations 
* SQL Joins
  * Challenge: SQL joins can take more than one line
  * Solution:
    * Small Data: There are several options here. You can use `N'...'` and do the join in numpy, you can use `N'...'` with `import pandas as pd` to do the join in pandas, or you can pipe data in and out of `join -t $'\t'`
    * Large Data: HDFS Joins
* Iterating a `ni` process over directories where the directory provides contextual information about its contents.
  * Challenge: This is something `ni` can probably do, but I'm not sure how to do it offhand.
  * Solution: Write out the `ni` spell for the critical part, embed the spell in a script written in Ruby/Python, and call it using `bash -c`.

# `ni` by Example Chapter 5 (alpha release)
Welcome to Chapter 4. At this point you have enough skills to read the other `ni` documentation on your own. As a result, this chapter should read a little briefer because it is focused on introducing you to the possibilities of each operator.

Unlike the other chapters thus far, this chapter has no theme; it's a list of useful operations. This chapter covers some of the interplay between `ni` and `bash`, HDFS joins using `nfu`, The `ni` monitor, cell operations, stream splitting, vertical column operations, sparse matrix operations, Ruby and Lisp operators, ni-specific perl operators, and has another Perl chapter.


## Calling`ni` from other programming languages

You've already been using `ni` within bash, so you've always been using `ni` from another langauge. In this section, we'll make the connections more explicit, and briefly discuss how to use `ni` from Ruby.

### `ni` and bash

[Spencer](https://github.com/spencertipping) refers to `ni` as Huffman-encoded bash, but we haven't given the treatment of `ni` and bash fully yet. If you're already familiar with bash, this will likely be review.

`e` also can use brackets rather than quotes to execute commands. However, this exposes the code that would have been quoted to bash, which might do something you don't want it to.

When you run a script without quotes, bash will first scan for metacharacters. So the script

```sh
$ ni e[seq 10 | grep 1]
```

is run as 

```sh
~/bin/ni "e[seq" "10" | /bin/grep "1]"
```

because bash will execute the pipe first.

The correct way to execute the script: 

```sh
$ni e'seq 10 | grep 1'
``` 

turns into 
```sh
exec("/bin/sh", "-c", "seq 10 | grep 1/")
```

This is a non-obvious feature of the bracketed version of `e`: `e[ word1 word2 ... wordN ]` turns into `exec("word1", "word2", ..., "wordN")`. You'll get shell character expansion with quotes, but not with brackets, the idea being that if you're using brackets, bash has already had a chance to expand the metacharacters like `$foo`.

### `ni` and Ruby
Aside from bash, Ruby feels like the best scripting language from which to call `ni`. Ruby has an easy and simple syntax for calling shell commands using backticks (i.e. `` `cmd` `` will run using the shell). 

Ruby backticks will execute using `/bin/sh` and not `/bin/bash`. There are a few caveats for running in `bin/sh`, which are covered in the [debugging docs](debugging.md).

### `ni` and Python

It's very easy to incorporate `ni` into Python is a very natural environment for adding `ni` as a command in your scripts.

```python
import subprocess

def ni(cmd):
	ni_cmd = ' '.join(cmd) if isinstance(cmd, list)
	subprocess.call("ni {}".format(ni_cmd))
```

### `ni` and Jupyter

`ni` can really shine in Jupyter; asynchronous processing turns out to be much easier in a Jupyter notebook than it is in a standard Python workflow.

Shelling out in Jupyter is really easy; simply prefix your command with 

Working in Jupyter, you'll want to set your 


## Stream Interleaving, Duplication, and Buffering
You've seen one of these operators before, the very useful `=\>`, which we've used to write a file in the middle of a stream. The way this works is by duplicating the input stream, and sending one of the duplicated streams silently to an output file. 


### `%`: Interleave Streams

The bare `%` will interleave streams as the data is output, and will consume both streams fully. For example `$ ni nE5fAA %[n100]` will output the second stream non-deterministically; it depends how fast that data is able to stream out.

You can also call `%#` with a positive number, in which case the streams will be interleaved with a ratio of `first stream : second stream :: #:1.` You can also call `%-#`, and the streams will be interleaved with a ratio: `first stream : second stream :: 1:#.`

Interleaving streams with a numeric argument will cut off the output stream when either stream is exhausted.


###  `=`: Duplicate Stream and Discard
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


### `B<op>`: Buffer operation

Likely the most important buffering operation is `Bn`, which buffers data to
null. This is especially useful in developing Hadoop Streaming jobs. Hadoop
streaming jobs require you to read the entire piece of data in the partfile;
this will error out if you try to run `r1000` to get a smaller chunk.

A similar operator is `Bd<size>`, which creates a bounded, disk-backed FIFO
buffer to add elasticity to a data pipeline. `Bd` doesn't modify the data at
all, but it may improve concurrency when you have two pipeline segments that
produce and consume data with different load patterns. `Bd` accepts size
specifications ending in nothing, `K`, `M`, `G`, `T`, or `P`, and optionally
followed by `B` after the unit. For example:

```bash
$ ni n100 ,sr+1         # n100 directly into ,sr+1
5050
$ ni n100 Bd64K ,sr+1   # n100 via 64KB of disk into ,sr+1
5050
```

This can be especially useful when sorting data, as `sort` intermittently blocks
the pipe to merge intermediate files.

```bash
$ ni nE6 ,sr+1
500000500000
$ ni nE6 Bd4MB ,sr+1
500000500000
```

FIFO buffer data is written verbatim; i.e. without compression. If you want the
buffer data to be compressed, you can surround it with `z` operators:

```bash
$ ni nE6 z Bd128K zd ,sr+1
500000500000
```


## Intermediate Column Operations
These operations are used to add columns vertically to to a stream, either by merging or with a separate computation.

### `w`: Append column to stream

`w` adds a column to the end of a stream, up to the minimum length of either stream.

```bash
$ ni ia ib ic w[n3p'a*a']
a	1
b	4
c	9
```

### `W`: Prepend column stream

`W` operates like `w`, except its output column is prepended. 

```bash
$ ni 1p'"a".."e"' p'split / /' Wn
1	a
2	b
3	c
4	d
5	e
```
  

### `v`: Vertical operation on columns

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

**Important Note**: As of 2017-03-30, this operator is too slow to use in production.

Also note that the Perl upper-case operator is written as `puc`, without quotes, as it's good `ni` style to do so.

### `Y` - dense-to-sparse transformation
`Y` Explodes each row of the stream into several rows, each with three columns:

* The index of the row that the input data that came from
* The index of the column that the input data came from
* The value of the input stream at the row and column specified by the first two columns.


```bash
$ ni i[operator could you help me] i[ place this call ] i[see the number on the matchbook]  FW Y r10
0	0	operator
0	1	could
0	2	you
0	3	help
0	4	me
1	0	place
1	1	this
1	2	call
2	0	see
2	1	the
```

### `X` - sparse-to-dense transformation
`X` inverts `Y`: it converts a specifically-formatted 3-column stream into a multiple-column stream. The specification for what the input matrix must look like is described above in the `Y` operator.

```bash
$ ni i[operator could you help me] i[ place this call ] i[see the number on the matchbook] FW Y r10 X 
operator	could	you	help	me
place	this	call
see	the
```

### `Z<n_cols>` - unflatten
`Z` takes data in the form of a single column and returns the same data reshaped into rows with the specified number of columns. Any overhanging data is pushed onto an incomplete row.

```bash
$ ni 1p'"a".."l"' Z4
a	b	c	d
e	f	g	h
i	j	k	l
```


## Join operations

`ni` implements two join operations, a join on sorted rows `j`, and a join on unsorted rows `J`. 


### `J` - streaming join unsorted rows
`J` is useful when the right side of your join is small enough to fit in memory, and does not require that either the left or the right side of your join be sorted. This is useful for most local operations, and joins against small data on the map side in Hadoop contexts. 




### `j` - streaming join sorted rows

`j` is useful 

Streaming joins are performed by matching two sorted streams on the value of their first column. 

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


## Cell Operations

Cell operations are similar to column operations, in that they are keystroke-efficient ways to do transformations on a single column of the input data.


### Hashing Algorithms
* `,h`: Murmurhash (deterministic 32-bit hash function)
* `,H`: Murmurhash and map the result into the unit interval.
* `,z`: Intify (hash and then convert hash values to integers starting with 1)
* `,m`: MD5 hash

Likely the most important of these functions is the deterministic hashing function, which does a good job of compacting long IDs into 32-bit integers.  This hashing should be good-enough for reasonable-sized data.

Using a little math, with ~40 million IDs, there will be only be about 1% hash collisions, and with 400 million IDs, there will be 10% hash collisions.  See [this](http://math.stackexchange.com/questions/35791/birthday-problem-expected-number-of-collisions) for an explanation.

Let's check that invariant:

```
$ ni n4E7 ,hA Cubuntu[o] uc
39814375
```

That means we had about 200,000 hash collisions in 40 million IDs, a rate of about .5%, which is good enough for our back-of-the envelope calculations.



### Cell Math Operations
* `,e<b>`: exponent b<sup>x</sup> -- deafults to e<sup>x</sup>
* `,l<b>`: logarithm (log<sub>b</sub>x) -- defaults to natural log
* `,j<amt>`: Jitter (add uniform random noise in the range `[-amt/2, amt/2]`)
* `,q<amt>`: Round to the nearest integer multiple of `<amt>`

These operations are mostly self-explanatory; jitter is often used for `ni --js` operations to create rectangular blocks of color.

```
# Test disabled for floating point imprecision.
$ ni n5 fAAAAAA ,eB ,eC2 ,lD ,lE3 ,eF ,lF
1	2.71828182845905	2	0	0	1
2	7.38905609893065	4	0.693147180559945	0.630929753571457	2
3	20.0855369231877	7.99999999999999	1.09861228866811	1	3
4	54.5981500331442	16	1.38629436111989	1.26185950714291	4
5	148.413159102577	31.9999999999999	1.6094379124341	1.46497352071793	5
```


### Column Math Operations
* `,a`: Running average
* `,d`: Difference between consecutive rows
* `,s`: Running sum 

You can use these `,a` and `,s` to get the average and sum of all data in the stream using, for example:

```bash
$ ni nE4 fAAA ,aA ,sB ,dC r~1
5000.5	50005000	1
```


## Numpy Operations

Writing Perl reducers is among the most challenging aspects of `ni` development, and it is arguably more error-prone than most other operations because proper reduction depends on an input sort. 

Moreover, Perl reducers are good with data, but they're still not great with math. If you had considered doing matrix multiplication in `ni` up to this point, you'd be pretty much out of luck. 

However, `ni` provides an interface to numpy (and all of the other Python packages on your machine), which gives you access to hugely powerful mathematical operations.  There are several associated costs, however: 1) the entire stream must be buffered into memory (though you can use partitioned matrix operations to get around this in some cases); 2) there are some arbitrary-feeling limitations on I/O; 3) the syntax is clunky compared to Perl.

It's great, you're gonna love it.

### `N'x = ...'`: Numpy matrix operations

The stream input to `N'...'` is converted into a matrix, where each row and column of the input is converted to a corresponding cell in a numpy matrix, `x`.

The values streamed out of `N'...'` are the values of `x`, so all operations that you want to do to the stream must be saved back into `x`. Compared to the Perl syntax, this is inelegant, and if `ni`'s gotten into your soul yet, it should make you more than a little frustrated.
 
However, the gains in power are quickly manifested:


```lazytest
# LazyTest: numpy isn't supported in all environments, as its API introduced
# breaking changes for binary loading around 2015
if ! [[ -e /nonumpy ]]; then
```

```bash
$ ni n3p'r map a*$_, 1..3' N'x = x + 1'
2	3	4
3	5	7
4	7	10
```

```bash
$ ni n5p'r map a . $_, 1..3' N'x = x.T'
11	21	31	41	51
12	22	32	42	52
13	23	33	43	53
```

```
$ ni n1N'x = random.normal(size=(4,3))'
-0.144392928715457      0.823863130371182       -0.0884075304437077
-0.696189074356781      1.5246371050062 -2.33198542804912
1.40260347893123        0.0910618083600519      0.851396708020142
0.52419501996823        -0.546343207826548      -1.67995253555456
```

```bash
$ ni i[1 0] i[1 1] N'x = dot(x, x.T)'
1	1
1	2
```

```lazytest
fi              # -e /nonumpy
```



### How `N'x = ...'` works
What `ni` is actually doing here is taking the code that you write and inserting it into a Python environment (you can see the python used with `ni e[which python]`

Any legal Python script is allowable, so if you're comfortable with `pandas` and you have it installed, you can execute scripts like:

```
ni ... N'import pandas as pd; 
		 df = pd.DataFrame(x); ... ; 
		 df.to_excel(...); 
		 x = df.reset_index().values;' 
		 ...
```

The last line is key, because the data that is streamed out of this operation must be stored in the variable `x`.  You can also use indented code within `N'...'`, and it will be processed correctly.

Also, like other operators, `N'...'` requires at least a row for input. `ni N'x = random.rand(size=(5,5)); x = dot(x, x.T)'` will return nothing, but adding a dummy row `ni n1N'x = random.rand(size=(5,5)); x = dot(x, x.T)'` will.

This is not (yet) the cleanest or most beautiful syntax [and that matters!], but it works.



## `m'...'`: Ruby
You have always had permission to use Ruby, but I've held off documenting it until you can do so responsibly. Why does Ruby require responsibility, whereas Python/numpy gets a pass (mostly)?

1. The Ruby driver operates in a streaming context, whereas the numpy environment `N` performs operations in-memory. As a result, the Python environment can do things that `ni` alone cannot do. The Ruby operators are weak-tea versions of cutting-edge Perl operators.
1. That means the primary use of Ruby in `ni` should be for its extensive and customizable libraries (i.e. gems). Thus, most of your Ruby code should look something like: `ni ... m'require "lib1"; require "lib2"; <do stuff with those libraries>'`
1. Because the primary use of Ruby is access to Ruby gems, your code becomes less portable. Here's a [good article](http://zachmoshe.com/2015/02/23/use-ruby-gems-with-hadoop-streaming.html) about using Ruby gems with Hadoop Streaming. It's really complicated!
1. Unlike Perl, where text is numbers, the Ruby driver requires that you explicitly cast to a datatype. Just saying, those extra keystrokes add up. Concretely: `ni n4m'r a, a + 1'` will complain about conversion of Fixnum to String without a proper cast.

### `m'a' ... m'q'`: Ruby Column Accessors

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

### `m'fields'`: Array of fields.
Analogous to `p'F_ ...'`. `fields` is a Ruby array, so you can use array syntax to get particular fields, for example: 

```bash
$ ni i[operator could you help me] i[ place this call ] i[see the number on the matchbook] FWr2m'r fields[0..3]'
operator	could	you	help
place	this	call
```

### `m'r ...'`: Print row
Analogous to `p'r ...'`.


## `l"..."`: Lisp

Lisp is in the same boat as Ruby, as a language that `ni` supports. Lisp operates in a streaming context, which is much better learned (and in most cases executed) in Perl. Lisp ends up even lower on the totem pole than Ruby because it can be a huge pain to install (on Mac, you have to bootstrap installation of SBCL by installing some other Lisp first).

So, if Ruby has gems, why even support Lisp? Here's why:

```bash
$ ni n4fAA l"(r (sr ('+ a) ('* b)))"
10	24
```

Streaming reduce is ugly in Perl, but smooth and easily understood in Lisp. There's something here, and it's worth working on.

### `l"a" ... l"q"`: Lisp Column Accessors

Analogous to the Perl and Ruby column accessors.

### `l"(r ...)"`: Print row

Analogous to `p'r ...'`; prints its arguments.

Look, if you're a damn Lisp programmer, you're smart enough to learn Perl. Just do that. I don't know Lisp. Go read these [docs](lisp.md).
  
  
## Conclusion
You've reached the end of chapter 5 of `ni` by Example, which coincides comfortably with the end of my current understanding of the language. Check back for more chapters to come, and more old ideas made fresh, useful, and fast.

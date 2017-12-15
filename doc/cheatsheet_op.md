# `ni` Operator Cheatsheet (alpha release)

## Preface

This cheatsheet is meant to be an exhaustive reference to the operators that compose `ni` as a language, except for its many Perl extensions (which have [their own cheatsheet](cheatsheet_perl.md)). This document is designed such that it can be read in order and it should make sense, though it's quite long and light on details.

However, there's a lot more to achieving fluency than just knowing the words; if you use `ni` regularly, you'll be well-rewarded for walking through [`ni` by Example](ni_by_example_1.md), the more formal tutorial. And if you ever write your own `ni` pipelines, you should read [`ni`-fu](ni_fu.md) to learn how to debug them. I guarantee it will save you time.

## Input Operations

* `n`: Integer stream
  * `n#`: Stream the sequence `1..#`, one number per line
  * `n0#`: Stream the sequence `0..#-1`, one number per line
  * `n`: Stream the sequence `1..`, one number per line.
* `<filename>`: File input
  * Automatically decompresses the file and streams out one line of the file out at a time.
  * Files are automatically decompressed from `gzip`, and `bzip`, `xzip` and `lzo` decompression are also supported if you have the binaries.
* `e[<script>]`: Evaluate script
  * evaluate `<script>` in bash, and stream out the results one line at a time.
  * Can also be written `e'<script'`
  * `$ ni n10 e'wc -l'` will count lines.
* `i<text>`: Literal text input
  * `$ ni iOK!` -- add the literal text `OK!` (and a newline) to the stream.
  * `$ ni i'a cat'` -- add the literal text `a cat` (and a newline) to the stream. The quotes are necessary to instruct the `ni` parser where the boundaries of the string are. Double quotes will work as well.
  * `$ ni i[these cats]` -- add a tab-separated line consisting of `these   cats`, and a newline.
* `D:<field1>,:<field2>...`: JSON Destructure
  * `ni` can easily grab scalar fields (i.e. strings, numbers, and nulls) from JSON, For example: `$ ni i'{"hi":5,"there":"alive"}' D:there,:hi` yields `alive	5`
  * The JSON destructurer does _not_ support pulling out list-based fields or nested hashes within JSON.
* `1`: Dummy pulse
  * `$ ni 1` is syntactic sugar for `$ ni n1`. It is useful for launching scripts in Perl, Ruby, Lisp, or Python from `ni`.
  * The `1` operator is primarily used to make a perl script run; it's often more useful in test than in actual development.

## File Operations and Compression
* ` \>filename`: Redirect stream to file and emit filename
  * Consumes the stream and outputs it to the file named `filename`, and emits the filename.
  * Note that there is **no whitespace** between the angle-bracket and the filename.
  * This "literal right angle bracket" operator `\>` is usually much more useful than a file redirect `>`, since the filename(s) output to the stream can be opened using the "literal left angle bracket" operator, `\<`.
* `\<`: Read from filenames
  * Normally when `ni` sees a filename in its instructions, it will open the file and `cat` its contents to the stream.
  * However, if you have a list of filenames, 
  * This tool can be powerful in combination with Hadoop operations, described later.
* `z`: Compress stream
  * Defaults to `gzip` compression, but `xzip`, `bzip`, and others are available. See [`ni` by Example Chapter 1](ni_by_example_1.md) for details.

## Basic Row Operations

The operator `r` is used to filter rows.

  * `$ ni <data> r3` - take the first 3 rows of the stream
    * **CAVEAT:** `r#` is a wrapper over the Unix utility `head`, and emits a `SIGPIPE` which will break Streaming MapReduce jobs. To use `r` in the context of MapReduce, use the safe row operator `rs` instead.
    * `$ ni <data> rs3` - take the first 3 rows of the stream and do not emit a `SIGPIPE`. 
  * `$ ni <data> r-3` - take everything after the first 3 rows of the stream
  * `$ ni <data> r~3` - take the last 3 rows of the stream
  * `$ ni <data> rx100` - take the first row in the stream, and every 100th row that follows. 
  * `$ ni <data> r.05` - sample 5% of the rows in the stream.
    * The sampling here is deterministic (conditioned on the environment variable `NI_SEED`) and will always return the same set of rows.
  * `$ ni <data> r/<regex>/` - take rows where `<regex>` matches.

## Basic Column Operations
Columns are referenced "spreadsheet-style"--the first column is `A`, the second is `B`, etc.

* `f`: Take columns
  * `$ ni <data> fAA` - select the first column and duplicate it
  * `$ ni <data> fAD.` - select the first column and all remaining columns starting with the fourth
  * `$ ni <data> fB-E` - select columns 2-5
  * `$ ni <data> fCAHDF` - selects columns 3, 1, 8, 4, 6, and streams them out in that order.
  * `$ ni <data> f#<N1>,#<N2>`, selects data from (zero-indexed columns <number1> and <number2>).
      * This can be used to select columns beyond the 26th column. `$ ni <data> f#87,#45,#9,#18` will take the 88th, 46th, 10th, and 19th columns from the data source.
      * Every column operation written with letters can be rewritten using the numeric form:
          * `$ ni <data> f#0,#0` is equivalent to `$ ni <data> fAA`
          * `$ ni <data> f#0,#3.` is equivalent to `$ ni <data> fAD.`
          * `$ ni <data> f#1-#4` is equivalent to `$ ni <data> fB-E`
          * `$ ni <data> f#2,#0,#7,#3,#5` is equivalent to `$ ni <data> fCAHDF`
* `r<cols>` - take row if exists
  * `$ ni <data> rCF` - take rows where columns 3 and 6 are nonempty.
* `F`: Split stream into columns
  * `F:<char>`: split on character
      * **WARNING**: this does not work with certain characters that need to be escaped; use `F/regex/` below for more flexibility (at the cost of less concision).
  * `F/regex/`: split on occurrences of regex. If present, the first capture group will be included before a tab is appended to a field.
  * `Fm/regex/`: don't split; instead, look for matches of regex and use those as
  the field values.
  * `FC`: split on commas (doesn't handle special CSV cases)
  * `FV`: parse CSV "correctly", up to newlines in fields.
  * `FS`: split on runs of horizontal whitespace
  * `FW`: split on runs of non-word characters
  * `FP`: split on pipe symbols
* `x`: Exchange columns
  * `x`: exchange the first two columns. 
      * `$ ni data x` is equivalent to `$ ni data fBA.`
  * `xC`: exchange column 3 with column 1. 
      * `$ni data xC` is equivalent to `$ ni data fCBA.`
  * `xBE`: exchange columns 2 and 5 with columns 1 and 2. 
      * This runs in order, so `B` will be switched with `A` first, and whatever is in the second column now will be switched with column `E`. 
      * `$ ni data xBE` is equivalent to `$ ni data fBECDA.`

  
## Sort, Unique & Count Operations

* **WARNING**: Sorting is often a rate-limiting step in `ni` jobs, as data will need to be buffered to disk if a sort is too large to fit in memory. If your data is larger than **1 GB**, you should consider distributing your workload using Hadoop operations.
* `g`: General sorting
  * `gB` - sort rows ascending by the lexicographic value of the second column
    * Lexicographic value is determined by the ordering of characters in the ASCII table.
    * `ni ia iC g` will put the capital `C` before the lower-case `a`, because capital Latin letters precede lowercase Latin letters in ASCII.
  * `gC-` - sort rows *descending* by the lexicographic value of the third column
   * `gCA-` - sort rows first by the lexicographic value of the third column, ascending. For rows with the same value for the third column, sort by *descending* value of the first column.
  * `gDn` - sort rows ascending by the *numerical* value of the fourth column.
  * `gEnA-` - sort rows ascending by the numerical value of the fifth column; in the case where values in the fifth column are equal, sort by the lexicographic value of the first column, descending.
    * **CAVEAT**: The numeric sort works on integers and floating-point numbers written as decimals. The numeric sort will **not** work on numbers written in exponential/scientific notation
* `u`: unique sorted rows
  * `$ ni <data> fACgABu`: get the lexicographically-sorted unique values from the first and third columns of `<data>`.
* `c`: count sorted rows
  * `$ ni <data> fBgc`: return the number of times each unique value of the second column occurs in `<data>`
  * Note that the above operation is superior to `$ ni <data> gBfBc` (which will give the same results), since the total amount of data that needs to be sorted is reduced.
* `o` and `O`: Numeric sorting
  * `o`: Sort rows ascending (numerical)
    * `oA` is syntactic sugar for `$ ni <data> gAn`
  * `O`: sort rows descending (numerical)
    * `OB` is equivalent to `$ ni <data> gBn-` 
  * **Important Note**: `o` and `O` sorts *cannot be chained together* or combined with `g`. There is no guarantee that the output of `$ ni <data> gAoB` will have a lexicographically sorted first column, and there is no guarantee that `$ ni <data> oBOA` will have a numerically sorted second column. If you want to sort by multiple conditions, you must use `g`.

## Cell Operations 
`$ ni <data> ,<op><columns>`

These provide keystroke-efficient ways to do transformations on a single column of the input data. Of particular use is the deterministic hashing function, which does a good job of compacting long IDs into 32-bit integers.


* `,a`: Running average
* `,d`: Difference between consecutive rows
* `,e`: Natural exponential (`e**x`)
* `,h`: Murmurhash (deterministic 32-bit hash function)
* `,j<amt>`: Jitter (add uniform random noise in the range `[-amt/2, amt/2]`)
* `,l`: Natural log (`ln x`)
* `,s`: Running sum 
* `,q`: Quantize
* `,z`: Intify (hash and then convert hash values to integers starting with 1)
* `,t`: Convert timestamp to readable ISO 8601 form
* `,g`: Geohash encode
* `,G`: Geohash decod3


## HDFS I/O & Hadoop Streaming
### How `ni` Interacts with Hadoop Streaming
When `ni HS...` is called, `ni` packages itself as a `.jar` to the configured Hadoop server, which includes all the instructions for Hadoop to run `ni`.

When `ni` uploads itself, it will also upload all data that is stored in data closures; if these closures are too large, the Hadoop server will refuse the job.

### Hadoop Operators
* `hdfs://<path>`: HDFS `cat`
  * Equivalent to `hadoop fs -cat <path>`
* `hdfst://<path>`: HDFS `text`
  * Equivalent to `hadoop fs -text <path>`
* `hdfsj://<path>`: HDFS `join`
  * Identifies the correct file within the directory `<path>` that should be joined with the map file (in a Hadoop Streaming context), `$ENV{mapreduce_map_input_file}` 
* `HS[mapper] [combiner] [reducer]`: Hadoop Streaming Job
  * Any `ni` snippet can be used for the mapper, combiner, and reducer. Be careful that all of the data you reference is available to the Hadoop cluster; `w/W` operations referencing a local file are good examples of operations that may work on your machine that may fail on a Hadoop cluster with no access to those files.
  * `_` -- skip the mapper/reducer/combiner. 
  * `:` -- apply the trivial operation (i.e. redirect STDIN to STDOUT) for the mapper/reducer/combiner
  * If the reducer step is skipped with `_`, the output may not be sorted, as one might expect from a Hadoop operation. Use `:` for the reducer to ensure that output is sorted correctly.
  * Remember that you will be limited in the size of the `.jar` that can be uploaded to your Hadoop job server; you can upload data closures that are large, but not too large.
* Using HDFS paths in Hadoop Streaming Jobs:
  * `ni ... ihdfst://<path> HS...`
  * The path must be input as literal text (with `i`) so that `ni` knows to get the data during the Hadoop job.
  * <span style="color:red">**WARNING**</span> if you do not use a literal path, for example with `ni hdfst://...`, `ni` will try to download all of the data, then upload the data to HDFS, and then run the job.
  * If you find that a job takes over a minute to start, you may want to check that you haven't made this error.


## SSH and Containers
* `s<host>[...]`: execute `[...]` in `<host>`
  * You will need to set up your hosts properly in your `.ssh/config` to use a named host. 
  * Remember that within the bracketed operator, you will have access to the `<host>` filesystem.
* `C<container_name>[...]`: execute `[...]` in `<container_name>`
  * Running in containers requires that Docker be installed on your machine.
  * Running containers can be especially useful to take advantage of better OS-dependent utilities.
  * For example, Mac OS X's `sort` is painfully slow compared to Ubuntu's. If you are developing on a Mac, there will be a noticeable performance difference increase if you replace: `ni n1E7 g` with `ni n1E7 Cubuntu[g]`, because the sort will be done in the faster Ubuntu container.
  * Containers are also useful for testing the portability of your code.


## Horizontal Scaling
Note that you will need sufficient processing cores to effectively horizontally scale. If your computer has 2 cores and you call `S8`, it may slow your work down, as `ni` tries to spin up more processes than your machine can bear.

* `S`: Horizontal Scaling 
  * `$ ni <data> S<# cores>[...]`: distributes the computation of `...` across `<# cores>` processors.

## Intermediate Column Operations
We can weave together row, column, and Perl operations to create more complex row operations. We also introduce some more advanced column operators.

* `j` - streaming join
  * This will (inner) join two streams using one or more of their columns as a key; if not specified, the key will be the first column  which is assumed to be sorted.
  * Adding columns to the operator will use those as the join column. `jAB` will join on the first two columns the left and the right datasets.
* `w`: Append column to stream
  * `$ ni <data> w[np'a*a']`
  * `w` will add columns only up to the length of the input stream
* `W`: Prepend column stream
  * `$ ni <data> Wn` - Add line numbers to the stream (by prepending one element the infinite stream `n`)
  * `W` will add rows only up to the length of the input stream
* `v`: Vertical operation on columns
  * **Important Note**: This operator is too slow to use in production.


## Data Closures and Checkpoints
Data closures are compiled into the `ni` source before the pipeline is executed. This allows them to serve the function of a broadcasted dataset across nodes in a distributed context. Closures can be accessed from within Perl snippets by using their name.

* `::closure_name[...]`: Create a data closure
  * A common motif for closures is using them as a filter:

* ```
$ ni ::good_points[i100 i3 i76] \
	n100 rp'^{%h = ab_ good_points} exists($h{+a})'
```

  * Here we've created a closure called `good_points` which contains the data `100\n3\n76\n` as a string (initially). We use some Perl (see the [Perl Cheatsheet](cheatsheet_perl.md) for details) to convert those lines into a Perl hash called `%h`, whose keys are  and we check whether the value of the first column `+a` exists as a key in the hash
  * Closures are computed separately from each other, which means that one closure cannot in general reference the value of another closure. If you find a situation where you need to create a closure that depends on the value of another closure, this is likely not (currently) good `ni` style, and you should look for another way to solve the problem.

* `@:[disk_backed_data_closure]`: Create disk-backed data closure.
  * A disk-backed data closure operates in much the same way as a regular data closure.
  * In general, backing your data closures to disk is a way to get around memory restrictions on machines when using particularly large data closures.
  * Disk-backed data closures may fail in environments where you have restrictions about writing to disk (_e.g._ in Hadoop mappers or reducers).
* `:<checkpoint_name>`: Create a checkpoint
  * Checkpoints are useful for building extended pipelines, especially ones with long-running or expensive operations (_e.g._ Hadoop MapReduce jobs).
  * The use of checkpoints is a bit too tricky for your author's taste, but you are likely smarter than he.
  * Here's an appropriate (if contrived) use of checkpoints:
     * `$ ni nE7 g :alpha_numbers r3`
     * With the checkpoint, the result of `$ ni nE7 g` will be stored in `:alpha_numbers`, and you can re-run the spell quickly.
     * Without the checkpoint, the entire sort would have to be re-run each time, which takes a painfully long amount of time.
     * In the background, `ni` is sinking the data from `$ni nE7 g` to a file called `alpha_numbers`. 
     * **<span style="color:red">WARNING</span>**: Once a checkpoint has been written once, it will remain the same. For example, if, after running `$ ni nE7 g :alpha_numbers r10` you run `$ ni n1000 g :alpha_numbers r10`, `ni` will start using the previous `alpha_numbers` checkpoint data. This failure mode is silent, so **BE CAREFUL!!** (Or don't use them, like me).
     * Checkpoints are also disk-backed, which means they suffer from many of the same limitations as disk-backed data closures. 

## Filename-Prepending File Operations
* `W\<`: Read from files and prepend filename
  * The `\<` operator will read data from files, but will not state which file each line came from.
  * `W\<` does the same thing as `\<` except it prepends a column to the output data with the name of the file the data came from.
* ` W\>`: Redirect filename-prepended stream to files
  * This operator consumes stream data with lines of the form `<filename> <data1> <data2> ... <dataN>` and outputs the lines `<data1> <data2> ... <dataN>` to `<filename>`, and outputs `<filename>`
  * Be aware that input data should be in sorted order; `ni i[a.txt 1] i[b.txt 2] i[a.txt 3]` will leave file `a.txt` with only one line with the value `3`.


## Matrix Operations
* `Y` - dense-to-sparse transformation
  * Explodes each row of the stream into several rows, each with three columns:
    * The index of the row that the input data that came from
    * The index of the column that the input data came from
    * The value of the input stream at the row + column specified by the first two columns.
* `X` - sparse-to-dense transformation
  * `X` inverts `Y`; it converts a specifically-formatted 3-column stream into a multiple-column stream.
  * In the case that there are collisions for locations `X`, `X` will sum the values
  * For example: `ni n010p'r 0, a%3, 1' X`
  * The specification for what the input matrix must look like is described above in the `Y` operator.
* `N'x = ...'`: Numpy matrix operations
  * Dense matrices can be transformed using Numpy-like operations
  * The entire input matrix (i.e. the stream) is referred to as `x`.
  * Example: `ni n10p'r map a*$_, 1..10' N'x = x + 1'` creates a matrix and adds one to every element with high keystroke efficiency.
  * Example `ni n5p'r map a*$_, 1..10' N'x = x.T'`
  * You also have available the entire numpy package at your disposal:
    * Example: `ni n10p'r map a*$_, 1..10' N'x = dot(x, x.T)'`
    * Example: `ni 1N'x = random.normal(size=(5,3))'`
  * Note that your statements must always store the matrix back in the variable `x`.

## Stream Operations

* `%<#>[stream]`: interleave streams
  * This is the most useful of the operations, especially for plotting data using `ni --js`.
  * `%`
  * **CAVEAT**: Interleaving is not an exact operation (though it's negligibly close for large datasets), and output can somewhat depend on the speed with which the two streams are generated.
* `=`: duplicate this stream and discard its output
  * The best use of this operation is to sink data to a file in the middle of a pipeline without impeding its progress.
  * `$ ni n100 =\>hundo.txt fAA p'r a*3, b' \>other.txt` will sink the stream after `$ ni n100` to a file called `hundo.txt`, while still allowing the data to be processed through the rest of the pipeline.
* `+`: append a stream
  * In general, a stream written later in the spell will be appended automatically to the stream that comes before it; I rarely use this.
  * `$ ni ia +ib` yields `a`, then `b`, the same as `$ ni ia ib` would. Not very useful.
* `^`: prepend a stream 
  * This is a more useful operator than `+` in theory, but it is also rarely used, since it in general makes more sense to order the streams.
  * `$ni ia ^ib` yields `b` then `a`. Slightly useful!



## Partitioned Matrix Operations
One improvement of `ni` over its predecessor, [`nfu`](github.com/spencertipping/nfu) is that mathematical operations on tall and wide matrices have better support through partitioning.

Some matrix operations are not performant in `ni` because they may require space greater than memory. If you're doing a lot of complex matrix operations, `ni` may not be the right tool. However, `ni` does provide some 

* `Y<col>`: Dense-To-Sparse Transform with Partitioning


```bash
$ ni i[a b c d] i[a b x y] i[a b foo bar] YC
a	b	0	0	c
a	b	0	1	d
a	b	1	0	x
a	b	1	1	y
a	b	2	0	foo
a	b	2	1	bar
```

`Y` has reduced over the first two columns of the data, and kept their values in the first two columns of its output. The next two columns represent the row and column in the matrix as the normal `Y` operator does, and and the final column has the value.

* `N<col>`: Numpy on Dense Partitioned Matrix

Reduces over the columns before `<col>`, and then does the standard `N` operation.

```bash
$ ni i[a b 1 5] i[a b 100 500] i[a b -10 -20] \
     i[c d 1 0] i[c d 1 1] \
      NC'x = dot(x.T, x)'
a	b	10101	50205
a	b	50205	250425
c	d	2	1
c	d	1	1
```

* `X<col>`: Sparse-To-Dense Transform of Partitioned Data

`X<col>` inverts `Y<col>`.

```bash
$ ni i[a b c d] i[a b x y] i[a b foo bar] YC XC
a	b	c	d
a	b	x	y
a	b	foo	bar
```


## Things other than Perl 

Don't use things other than Perl. Here are other things:

*  `m'<...>'`: Ruby
   * applies the Ruby snippet `<...>` to each row of the stream 
*  `l'<...>'`: Lisp
   * applies the Lisp snippet `<...>` to each row of the stream 

I have only ever used the Ruby extension, and only to us a library not written in `ni`.

Keep in mind that code written in any other language will not be portable and result in configuration headaches. For example, if you have a Ruby gem installed on your local machine and are able to run a `ni` spell on your local, you will have to install the same gem on your remote machine to use it over SSH. Moreover, if you want to run the same task on your Hadoop cluster, you'll have to have the gem installed on every node of the cluster 

When I need another language for its library, I'll usually create a copy of the (part of the) library that I need and add it to `ni` instead.


## Binary Operations
The primary use of binary operations is to operate on data that is most effectively represented in raw binary form (for example, `.wav` files). See the [binary docs](binary.md) until I figure out something useful.

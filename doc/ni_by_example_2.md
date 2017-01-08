#`ni` by Example, Part 2 (WIP)

Welcome to the second part of the tutorial. At this point, you should be familiar with fundamental row and column operations; sorting; basic Perl operations; file I/O and comopression; and some basic principles of `ni` style. Before continuing, it's advisable to read over the first part of the horribly misnamed [cheatsheet](cheatsheet.md) to see some of the operations that were glossed over.

The key concept that we will cover (and really, the key to `ni`'s power) is the ability of `ni` to package itself and execute in-memory on remote machines. To that end, we will explain the use of `ni` on local Docker instances; over `ssh` on remote machines, and how to use `ni` to write simple and powerful Hadoop Streaming jobs. 

Other key concepts for this tutorial include streaming reduce operations, data closures, and cell operations. We'll also cover more `ni`-specific Perl extensions, and some important parts of Perl that will be particularly useful in `ni`.

Before we get into anything too useful, however, we need to take a detour into how `ni` works at a high level. It's not completely necessary to know this in order to use `ni`, but understanding this will help you think like `ni`.

##Quines
A _quine_ (pronounced: KWINE) is a program that prints its source code when it is run. If you haven't run into quines before (or the equivalent terms selfrep or self-representing program), and you go out and start looking at them, they can be mind-bending and near-impossible to read. That is the correct reaction; you should start becoming comfortable with that feeling.

We'll write a classic quine in Scheme (or Lisp), and then a quine in Perl.

####Scheme/Lisp mini-tutorial
If you're already familiar with Lisp syntax, skip ahead to the next section. If you're not familiar with either of those languages, they're much more worth learning than `ni`, but it's probably more urgent that you learn `ni` for some reason, so this mini-tutorial will teach you enough to understand our first example quine.

Start by checking out [repl.it](http://www.repl.it). Select Scheme and you'll be taken to a Scheme REPL.

Here's what you need to know:

* Function applcation in Scheme/Lisp starts with an open parenthesis.
  * `(+ 1 2)` yields `3`. 
  * `(* 5 6 7)` yields `210`
* Scheme lists are written in parentheses and are delimited by spaces.
* The function `list` returns a list of its arguments:
  * `(list 1 3 5)` yields `(1 3 5)`
* The function `quote` takes one argument and returns its literal value:
  * `(quote 5)` yields `5`
  * `(quote (list 1 2 3))` yields `(list 1 2 3)`--note that the list function was not evaluated.
* `lambda` defines an anonymous function with any number of named parameters.
  * `(lambda (u v) (u + v))` yields a closure. There's a lot of ink spilled on what a closure is and does. For the purpose of this tutorial, you can think of a closure as a a function that has not been supplied its arguments.
  * `((lambda (u v) (u + v)) 4 5)` yields 9, because the 4 and the 5 are passed as arguments to the lambda.

####A simple quine
Let's build a quine, starting with this piece:

```
(lambda (x)
   (list x (list (quote quote) x))
```

`lambda` creates an anonymous function which, on input x, returns a list with first element `x`. The second element of the list is also a list. The first element of the sublist is the result of  `(quote quote)`, and the second element is `x`. To make the operation of this function more concrete, head over to  and look at what happens when we do the following:

```
((lambda (x)
    (list x (list (quote quote) x)))
  1)
=> (1 (quote 1))
```
So when we apply ths function to an argument, we get back the argument, followed by a list consisting of `quote` and the argument.

Already that should feel like a step in the right direction; we have a function that takes a piece of data, reproduces that data and then a quoted representation of that data. In quines with this structure, this function is often referred to as "the code."

What we need now is the appropriate piece of data to feed to the code. We know our data has to reproduce our code, so that means what we feed to our code must be a string representation of itself. We do this by applying `quote` to the text of the code.

```
((lambda (x)
  (list x (list (quote quote) x)))
 (quote
  (lambda (x)
   (list x (list (quote quote) x)))))
   
=> ((lambda (x) (list x (list (quote quote) x))) (quote (lambda (x) (list x (list (quote quote) x)))))
```

Note that not all quines are structured this way

For those inclined to theoretical computer science, [David Madore's tutorial on quines](http://www.madore.org/~david/computers/quine.html) is excellent.  For more examples quines, see [Gary P. Thompson's page](http://www.nyx.net/~gthompso/quine.htm)

####A quine in Perl

```
#!/usr/bin/perl
eval($_=<<'_');
print "#!/usr/bin/perl\neval(\$_=<<'_');\n${_}_\n"
_
```

Copying the lines and running: 

```
$ pbpaste > quine.pl
$ cat quine.pl | perl
#!/usr/bin/perl
eval($_=<<'_');
print "#!/usr/bin/perl\neval(\$_=<<'_');\n${_}_\n"
_
```

This quine looks odd, but most of the trickery is done using the heredoc syntax (which is a way of writing multi-line strings starting with `<<`). Heredocs are parsed in a non-obvious way; at a high level, the code works like this:

1. `$_` becomes the code written as a heredoc, which is just the 3rd line. The heredoc parser knows to skip the end of the line.
2. `eval $_` happens, causing that code to be executed
3. inside `eval $_`, `print "#!... ${_}..."` happens -- notice the reference back to `$_`, which contains the code being evaled

The key here is that because the code is inside a single-quoted heredoc, it can be interpolated directly into its own representation.

####Quines, so what?

When studying quines, most of the examples you see don't do anything (other than print themselves), which should make us ask why they're even worth studying.

Consider what happens when we pipe the output of a quine back to an interpreter. Copying our quine from above

```
$  cat quine.pl | perl | perl | perl
#!/usr/bin/perl
eval($_=<<'_');
print "#!/usr/bin/perl\neval(\$_=<<'_');\n${_}_\n"
_
```

We can keep connecting output pipes to input pipes and getting the same output. If we think about pipes more generally, we might imagine taking a quine, passing its output as text over ssh, and executing that quine using perl on another machine. A quine can be passed from machine to machine, always with the same output.


##`ni` is a quine

A quine is a program that prints its source code when run, and ni is (essentially) a quine. To get `ni` to print its source, run:

`$ ni //ni`

```
#!/usr/bin/env perl
$ni::self{license} = <<'_';
ni: https://github.com/spencertipping/ni
Copyright (c) 2016 Spencer Tipping | MIT license

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
....
```

A few things to think about here, but the most important thing is, "so what?" `ni` can print it's own source code; it's not hard to get a python program to print its source. 

The `<<'_'` starts a multi-line string in Perl. It borrows from the heredoc syntax in `bash`, and starts 





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




##Perl for `ni` fundamentals



##Connecting `bash` and `ni`

* `e[<script>]`: Evaluate script
  * evaluate `<script>` in bash, and stream out the results one line at a time.

##Input Operations
* `D:<field1>,:<field2>...`: JSON Destructure
  * `ni` implements a very fast JSON parser that is great at pulling out string and numeral fields.
  * As of 2016-12-24, the JSON destructurer does not support list-based fields in JSON.

####`ni` and directories

Enrichment sections explore relevant but non-critical `ni` functions. Their contents are not critical to productive `ni` development, and they can be skipped or skimmed.

Start by making some data (Note that you have to be in `bash` for the echo statements to work. [Here](http://stackoverflow.com/questions/8467424/echo-newline-in-bash-prints-literal-n) is a very interesting post about `echo`'s unintuitive behavior):

```
$ rm -rf dir_test && mkdir dir_test
$ echo -e "hello\n" > dir_test/hi
$ echo -e "you\n" > dir_test/there
```

Let's start with `$ ni test`

```
dir_test/hi
dir_test/there
(END)
```

`ni` has converted the folder into a stream of the names of files (and directories) inside it. You can thus access the files inside a directory using `\<`.

`$ ni dir_test \<`

yields:

```
hello
you
(END)
```

`ni` also works with the bash expansion operator `*`.

For example, `ni dir_test/*` yields:

```
hello
you
(END)
```

`ni` is able to go to the files directly becasue it is applies bash expansion first; bash expansion generates the file paths, which `ni` is then able to interpret.

```
$ ni --explain dir_test/*
["cat","dir_test/hi"]
["cat","dir_test/there"]
```


  
##Understanding the `ni` monitor
More details [here](monitor.md). Overall:

* Negative numbers = non-rate-determining step
* Positive numbers = rate-determining step
* Large Positive numbers = slow step


##SSH and Containers

* `C<container_name>[...]`: execute `[...]` in `<container_name>`
  * Running in containers requires that Docker be installed on your machine.
  * Running containers can be especially useful to take advantage of better OS-dependent utilities.
  * For example, Mac OS X's `sort` is painfully slow compared to Ubuntu's. If you are developing on a Mac, there will be a noticeable performance difference increase by replacing:
    * `ni n1E7 g` with
    * `ni n1E7 Cubuntu[g]`
  * Containers are also useful for testing the portability of your code.
* `s<host>[...]`: execute `[...]` in `<host>`
  * You will need to set up your hosts properly in your `.ssh/config` to use a named host. 
  * You will want to do this to reduce keystrokes.
  * Remember that within the bracketed operator, you will have access to the `<host>` filesystem.
  * `ssh` zips its output before transfer over the network which will decrease overhead and increase speed in general (but not always).


##Intermediate Column Operations
We can weave together row, column, and Perl operations to create more complex row operations. We also introduce some more advanced column operators.

* `w`: Append column to stream
  * `$ ni <data> w[np'a*a']`
  * `w` will add columns only up to the length of the input stream
* `W`: Prepend column stream
  * `$ ni <data> Wn` - Add line numbers to the stream (by prepending one element the infinite stream `n`)
  * `W` will add rows only up to the length of the input stream
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
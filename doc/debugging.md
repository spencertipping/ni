# Debugging

As a result of `ni`'s concision and the compactness of its namespace, even experienced developers can have difficulty eyeballing errors. However, `ni`'s structure as a stream-processing language makes development and debugging both very easy.

## Compilation Errors
Generally these will stem from an error with some of your Perl. Ask someone for help.

## Job is too slow
`ni` is a stream-processing language; every job should be fast (or at least, start spitting out output fast).  See [optimization.md](optimization.md) for tips on speeding up slow jobs.


## Parsing Errors

### Bracketed operators
If you get an error involving a bracketed operator and you're sure you've written everything correctly, try inserting whitespace after the opening bracket and before the closing bracket. `ni` is usually good about telling you when that's necessary.

### Hadoop Streaming
Take a look at the following snippet:

`$ ni n1 HS [p'r a'] _ _`

It fails with the error: `ni: failed to parse starting here (ni --dev/parse to trace):  _`
  
Running $ `ni --dev/parse n1 HS [p'r a'] _ _`

```
<HUGE NUMBER OF LINES OF PARSE TRACE>
 ni::parse [[/series], n1, HS, [pr a], _, _] = [[[n, 1, 2], [hadoop_streaming, [], [[perl_mapper, r a]], undef]], _]
```

The `ni` parser interprets the next character after the `S` in `HS` as the mapper job, which we had meant to be `p'r a'`; 


## Hadoop Streaming Errors
The first thing to do when hunting for errors in hadoop streaming jobs is to make sure that your mapper, combiner, and reducer run outside of an `HS` context. If they do, then 

### Hadoop Streaming .jar too large
When `ni` runs hadoop streaming jobs, it sends **itself and all data** to the job server. If you have a large data closure, that closure will be included in the payload sent to the job server. If your closure is larger than a couple hundred megabytes, you should try a different approach to the problem (e.g. use an HDFS join [coming soon to `ni`, use [nfu]`nfu` for now]).

### Hadoop Job killed by `SIGPIPE`
Hadoop jobs must buffer all of their input, so you cannot use a bare `r1000` within a mapper, combiner, or reducer within `HS`. Instead, use `Bnr1000`, which will buffer excess input to null.

### Too many partfiles
The YARN resource manager will refuse to accept jobs that use too many (for example, >100K) partfiles as input; if you have too many partfiles, then you can use the `HS` syntax to run multiple identical jobs over time.

## Hadoop Streaming Job is Slow
Likely it's that you have a slow step 

### Reading the Hadoop Logs
The montior for Hadoop jobs includes the unix timestamp; this will be in the `stderr` logs. 

### Many killed tasks "Container preempted by scheduler"

In this case, the tasks that you are running are taking too long. The "ideal" mapreduce task takes something in the range of minutes to run. Once you've gone up to over an hour, you're beyond the pale of what you *should* be doing. Split your data up into a larger number of partfiles using `^{mapreduce.jobs.reduces=<lots>} HS:_:`, or use the upcoming hadoop repartition operator.

## Hadoop Streaming Erroneous Output

### Modifying the sort column
```sh
$ ni ihdfst:///<path> HS[p'r a =~/^(.{8})/, FR 1' ] \
                        [p'@ls = sort {f_($b) <=> f_($a)} reA; $n = min($#ls, 10); @ls[0..$n-1]' ] \
                        :
```

The code above is designed to get the top values based on column `f` of some
data. However, when this code is run, it was found that a bunch of the data
idiosyncratically disappeared, and that some of the output had more than  find
that is a case of the classic bug optimizing too early.


## `ni` is installed but my machine can't find it

Quick rundown of the obvious causes:

1. `ni` isn't installed; head over to your `ni` directory and execute `./build`
2. `ni` isn't linked; from the `ni` directory execute `ln -s $PWD/ni ~/bin/ni` to install it for your user or `ln -s $PWD/ni /usr/bin/ni` for all users.
3. The folder where you linked `ni` isn't in your `$PATH`. Check your `.bash_profile`.

Now onto non-obvious causes:

### Running in `bin/sh`, with Ruby backticks, as a Python subprocess, etc.

For most purposes bash and sh will behave the same way for `ni` -- the main exceptions are things like `{x..y}` (a bash-ism) and some environment-variable expansion forms. In particular, `bin/sh` may not like some of your environment variables, especially those that use `~` for the home directory. 

A quick way to check for the above is to run `which ni`, which should use `bin/sh` rather than `bin/bash`. If nothing shows up, but you are still able to run `ni` from `bash`, you probably need to change `~/bin` in your `$PATH` to `$HOME/bin` so `bin/sh` can see it.

If you do run into a bash-specific case, you can create your command as a string `cmd` and run it (in Ruby) with `bash -c "#{cmd}"`.

## Erroneous Output
The other important type of error to debug are ones that run completely but give the wrong output. Here are some commmon pitfalls you might run into.

### Sorting doesn't work how I want
As of 2017-01-22, sorting works in a way that is unintuitive on **certain** operating systems. Specifically, `ni`'s `g` operator ignores blanks (i.e. separators between columns) on certain operating systems (not Mac or Ubuntu). In general, if you need to sort by a particular column of the data, you will be better served using `g<column name>`.

### Referencing the wrong column after re-indexing

Running `ni n5 p'r a, a, a, a' fBCD p'r b*3 + c*2 + d'` gives multiples of 5, and not multiples of 6 as you might expect. The reason is that once `fBCD` is run, the columns are re-indexed such that the column that was `B` would now be accessed by `A` (or `a`) in a Perl context. 

Note that `ni` allows you to use columns that are undefined. For example, `ni n5 xC` will give you data with two empty columns followed by the input stream.

### Spelling Errors
When developing `ni` pipelines, it is often the case that the name of one data closures will change in one place but not another.

### Quoted HDFS Paths for Hadoop Streaming
Note that when using a HDFS paths for Hadoop Streaming jobs, you __must*__  quote the input path from 

*Okay, it might work if you don't quote the path, but it'll take [forever](optimization.md).

### Perl 
If something's going wrong in a Perl context, but it looks right, try being more explicit (for example, using `b()` rather than `b` to reference the second field).

## I can get it to work, but... 

### ... it's not intuitive!
Two possibilities here: 

1. You're right. `ni` should be intuitive to experienced developers with a moderate Perl background (note: raw Perl code not guaranteed to be intuitive). Talk to one of the developers. This is how we got the hash-construction syntactic sugar.
2. Your intuition is wrong, and there's an even more intuitive solution that you haven't yet even heard of yet.  Reach out.  

### ... I'm typing the same thing over and over

One of the goals of `ni` is keystroke-efficiency; if you have a general-enough operator, speak to one of the developers and it may be added as a feature to the language.

## Easily Confused Operators

* `f` and `F`
  * `f` selects columns, and will always be followed by a number of capitalized alphabetical characters
  * `F` is used to split a single column of raw text into columns.
* `a`, `a()`, and `a_`
  * `a` and `a()` are identical operators, returning **the first element of a tab-separated row**. `a` is more common, and works everywhere except when the context is unclear (and `a` alone would be interpreted as a string). In this case, use `a()`.
  * `a_` is an operator on arrays of lines of the stream, returning the first element of each line **as an array**. These arrays can be operated on by buffered readahead reducers.


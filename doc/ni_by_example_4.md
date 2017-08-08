# `ni` by Example, Chapter 4 (alpha release)

Welcome to the fourth part of the tutorial. At this point, you should be familiar with fundamental row and column operations; sorting, I/O and compression. You've also covered the basics of Perl, as well as many important `ni` extensions to Perl.

The key concept that we will cover (and really, the key to `ni`'s power) is the ability of `ni` to package itself and execute in-memory on remote machines. To that end, we will explain the use of `ni` on local Docker instances; over `ssh` on remote machines, and how to use `ni` to write simple and powerful Hadoop Streaming jobs. 

Other key concepts for this tutorial include streaming reduce operations, data closures, and cell operations. We'll also cover more `ni`-specific Perl extensions, and some important parts of Perl that will be particularly useful in `ni`.

Before we get into anything too useful, however, we need to take a detour into how `ni` works at a high level. It's not completely necessary to know this in order to use `ni`, but understanding this will help you think like `ni`. 

## `ni` is self-modifying

`ni` is written in [self-modifying Perl](https://github.com/spencertipping/writing-self-modifying-perl), and the ability to rewrite its source code is the key to its virality. In biological terms, it is useful to think of `ni` is truly viral; it can be run in-memory on any machine with bash and Perl.


### `ni` evaluation basics
Part of the reason `ni` spells are easy to build is because they are pipelined by default, and in particular, they are pipelined with Unix pipes; the output of one `ni` operation is piped as input to the next operation.

```
ni <op1> <op2> <op3> ... <opN>
``` 
is, for the most part, equivalent to 

```
ni <op1> | ni <op2> | ni <op3> | ... | ni <opN>
```

`ni --explain` works by telling you what each `<op>` above is.

However, this isn't quite the whole story. 

### `::closure_name[...]`: Create a data closure

```bash
$ ni ::five[n5] n3p'r a, five'
1	12345
2	12345
3	12345
```
Any `ni` operations executable on the machine from whose context `ni` is being executed can be turned into a data closure. This caveat will become more important when we start using `ni` to execute on machines other than the ones we develop on.  The closure can be referenced within a Perl snippet as  `p'... closure_name ...'`

```bash
$ ni --explain ::five[n5] n3p'r a, five'
["memory_data_closure","five",[["n",1,6]]]
["n",1,4]
["perl_mapper","r a, five"]
```

In chapter 1, we described `ni` evaluating different operators as follows:

`$ ni <op1> <op2> ... <opN> == $ ni <op1> | ni <op2> | ... | ni <opN>`

Data closures provide a counterexample to the basics of `ni` evaluation written above. 

```bash
$ ni ::five[n5] | ni n3p'r a, five'
1	five
2	five
3	five
```

The reason that the example including pipes gives different results than the example with no pipes is that **creating the data closure modifies `ni` itself**.  In the piped example, the first `ni` is modified but is not used; the second `ni` is your system `ni`, which remains unmodified. The second `ni` therefore cannot access the data closure built in the first `ni`.


### Data Closure Evaluation and `ni` self-modification
Data closures, regardless of where they are written in a ni spell, will be evaluated before everything else, and in the order that they are written.

That means that `$ ni ::five[n5] n3p'r a, five'` ie equivalent to `$ ni n3p'r a, five' ::five[n5]`, even though in the latter, it looks like it's referencing something that was computed later in the pipeline.

Data closures are (or will be shortly implemented such that they are) computed in order; that means you can use one data closure to compute another, so long as the one computed from is expressed in the stream before the one which uses it to do some computation.

We can rewrite a `ni` pipeline a little more accuartely as the following:

```sh
$ ni ::dataclosure1 ... ::dataclosureM <op1> <op2> ... <opN> 
```

```sh
$ ni ::dataclosure1 ... ::dataclosureM (export to) ni_prime
$ ni_prime op1 | ni_prime op2 | ... | ni_prime opN
```

We will see how `ni` actually does this in the following sections.


## `ni` is a quine

A _quine_ (pronounced: KWINE) is a program that prints its source code when it is run. If you haven't run into quines before (or the equivalent terms selfrep or self-representing program), and you go out and start looking at them, they can be mind-bending and near-impossible to read. That is the correct reaction; you should start becoming comfortable with that feeling.

We'll write a classic quine in Scheme (or Lisp), then a quine in Perl, and then demonstrate that `ni` is a quine without getting too deep into the details.

### Scheme/Lisp micro-tutorial
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
  * `(quote (list 1 2 3))` yields `(list 1 2 3)`. Note that the list function was not evaluated.
* `lambda` defines an anonymous function with any number of named parameters.
  * `(lambda (u v) (u + v))` yields a lambda (i.e. an anonymous function) that adds two values.
  * `((lambda (u v) (u + v)) 4 5)` yields 9, because the 4 and the 5 are passed as arguments to the lambda.

### A simple quine in Scheme
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

Note that not all quines are structured this way (with code and data to interpret the code), but these are the simplest types of quines to write and explain.

For those inclined to theoretical computer science, [David Madore's tutorial on quines](http://www.madore.org/~david/computers/quine.html) is excellent.  For more examples quines, see [Gary P. Thompson's page](http://www.nyx.net/~gthompso/quine.htm).

### A quine in Perl

```
#!/usr/bin/perl
eval($_=<<'_');
print "#!/usr/bin/perl\neval(\$_=<<'_');\n${_}_\n"
_
```

This code uses heredoc syntax, which is a way of writing multi-line strings in bash, POSIX, and Perl (and other Perl-influenced languages like PHP and Ruby). Enrichment on heredocs is available[... here](http://www.tldp.org/LDP/abs/html/here-docs.html).

Heredocs start with `<<` followed by a delimiter which is the instruction to the interpreter of where the string stops. In this case, the delimiter is the character `_`. Surrounding the delimiter with single quotes, as above, allows for string interpolation within heredocs; without these quotes around the delimiter, no string interpolation is allowed.

Heredocs can be parsed in non-obvious ways, and the non-obvious parsing is used in this quine. The heredoc can be parsed starting on the line after the `<<` and the delimiter, and that is what is used here. Due to the parsing, the string `print "#!/usr/bin/perl\neval(\$_=<<'_');\n${_}_\n"` is stored into `$_`.


If you found the previous paragraphs on heredocs inscrutable, don't worry too much because it's not so important; like the quine we saw in the previous section, this quine is composed of code:

```sh
#!/usr/bin/perl
eval($_=<<'_');
print 
```
and data:

```sh
"#!/usr/bin/perl\neval(\$_=<<'_');\n${_}_\n"
_
```

What makes this quine a bit more difficult to read is that there is some overlap between the code and the data involving the assignment statement. Also, it's Perl.


Copy the lines into a file `quine.pl` and run: 

```sh
$ cat quine.pl | perl
#!/usr/bin/perl
eval($_=<<'_');
print "#!/usr/bin/perl\neval(\$_=<<'_');\n${_}_\n"
_
```

Most of the trickery is done using the heredoc syntax, which are parsed in a non-obvious way. At a high level, the code works like this:

1. `$_` is set to the string value prescribed by the heredoc; since the heredoc 
2. `eval $_` happens, causing the string of code written in the third line.
3. inside `eval $_`, `print "#!... ${_}..."` happens -- notice the reference back to `$_`, which contains the code being evaled

The key here is that because the code is inside a single-quoted heredoc, it can be interpolated directly into its own representation.

### Quines, so what?

When studying quines, most of the examples you see don't do anything (other than print themselves), which should make us ask why they're even worth studying.

Consider what happens when we pipe the output of a quine back to an interpreter. Copying our quine from above into `quine.pl`:

```
$  cat quine.pl | perl | perl | perl
#!/usr/bin/perl
eval($_=<<'_');
print "#!/usr/bin/perl\neval(\$_=<<'_');\n${_}_\n"
_
```

We can keep connecting output pipes to input pipes and getting the same output. If we think about pipes more generally, we might imagine taking a quine, passing its output as text over ssh, and executing that quine using perl on another machine. A quine can be passed from machine to machine, always with the same output; it is a fixed point of the code under the evaluation/interpretation operator.



### `//ni`: Print `ni`'s _current_ source
As a reminder, you should be using a vanilla (to the greatest extent possible) bash shell for these commands. If you're not running a bash shell, `bash` at the terminal will pop you into a bash shell.


`ni` is a quine. To get `ni` to print its source, run:

```sh
$ ni //ni
#!/usr/bin/env perl
$ni::self{license} = <<'_';
ni: https://github.com/spencertipping/ni
Copyright (c) 2016 Spencer Tipping | MIT license
....
_
....
```

Like the example Perl quine above, `ni` uses the tricky interpretation of the heredoc syntax, and the semicolon at the end of the line is not subsumed into the multi-line string. Also, because `ni` is self-modifying, `//ni` cannot be said to print `ni`'s source per se; instead it prints `ni`'s source at the time that it is called.

This provides us with a surprising power. We can now execute `ni` indirectly by piping `ni`'s source to `perl -`. The `-` instructs perl to read code from the command line.  More generally,

> `$ ni ...` is equivalent to `ni //ni | perl - ...`

If we think about a pipe more generally, as passing data not just from one process to another, but from one machine to another, you should envision the possibilities of offloading work from one machine to another that's better equipped to solve your target problem.


## `ni` is a self-modifying quine

In the section on `ni` being self-modifying, it was mentioned that `ni` execution follows a structure something like this:

```sh
$ ni ::dataclosure1 ... ::dataclosureM <op1> <op2> ... <opN> 
```

is equivalent to

```sh
$ ni ::dataclosure1 ... ::dataclosureM (export to) ni_prime
$ ni_prime op1 | ni_prime op2 | ... | ni_prime opN
```

In fact, this `ni_prime` is a local modification of `ni`, which incorporates data closures. The details are outside the scope of how this occurs are outside the scope of this tutorial, but here's some evidence that this is going on.

```sh
$ ni //ni | wc -c
  743514
```

```sh
$ ni ::my_closure[n10] //ni | wc -c
  743569
```

If we've really inserted a data closure into `ni` as a quine, `ni` is really a quine, then we should be able to execute it, for example, by passing the code to perl.

```bash
$ ni ::five[n5] //ni | perl - 1p'five'
1
2
3
4
5

```

This is really quite magical; we've taken `ni`, made a simple but powerful modification to its source, then passed the entire source to `perl` (which had no idea what it would receive), and it was able to access something that doesn't exist in the installed version of `ni`. It is because `ni` is a quine that allows it to be self-modifying.


## SSH, Containers, and Horizontal Scaling

We've covered why `ni` can be indirectly executed on the same machine using the identity `$ ni ...` == `$ ni //ni | perl - ...`. The natural next steps are to explore indirect execution of `ni` scripts on virtual machines and on machines you control via `ssh`. While horizontal scaling (running a process on multiple cores) has nothing to do with the indirect execution in containerized operations, it has functional and semantic similarity with these other operators in this section.

### `C`: execute in a container

Running in containers requires that Docker be installed on your machine. It is easy to install from [here](https://www.docker.com/).

Running containers can be especially useful to take advantage of better OS-dependent utilities. For example, Mac OS X's `sort` is painfully slow compared to Ubuntu's. If you are developing on a Mac, there will be a noticeable performance change using `$ ni n1E7 g` vs `$ ni n1E7 Cubuntu[g]`.


### `s`: execute over `ssh`

You will need to set up your hosts properly in your `.ssh/config` to use a named host. For example, if you log in with the command `ssh user.name@host.name:port.number`, you would create an alias for that host by entering the following lines in your `.ssh/config` 

```
host <alias>
    HostName <host.name>
    Port <port.number>
    User <user.name>
```

You would access this as `$ ni ... s<alias>[...]`. The alias used in most of the other `ni` docs is `dev`.

Inside the brackets, you will have access to the filesystem of the remote machine (but not the machine from which `ni` was originally called). 

### `S`: Horizontal Scaling 
Remember that `ni` should be I/O bounded; as such, `ni` provides a very easy interface to multiprocessing using the horizontal scaling operator, `S`. 

`$ ni S<# cores>[...]`: distributes the computation of `...` across `<# cores>` processors. 

Running an operator with `S8` on a machine with only two cores is not going to give 8x the computing power. In fact, if you request more cores than you have, you'll likely end up slowing your progress.  

  
## Hadoop Streaming MapReduce

`ni` and MapReduce complement each other very well; in particular, the MapReduce paradigm provides efficient large-scale sorting and massive horizontal scaling to `ni`, while `ni` provides significant concision and progrmmer ease in comparison to writing and submitting scripts. `ni` is often also low-overhead and very fast when written appropriately.

### MapReduce Fundamentals

MapReduce landed with a splash in 2004 with this [excellent paper](https://static.googleusercontent.com/media/research.google.com/en//archive/mapreduce-osdi04.pdf) by Jeffrey Dean and Sanjay Ghemawat of Google and (with Hadoop) in many ways ushered in the era of "big data."

To understand how to best use `ni` with MapReduce, it's important to understand how it works.

The MapReduce paradigm breaks down into three steps:

1. Map
2. Combine (optional)
3. Reduce

In general, a mapper will read in large, often unstructured data, and output  more highly structured information, which will be combined into statements about the original data by the reduce operation.  Because both map and reduce occur across many processors, there is often high network overhead transferring data from mappers to reducers. A combiner is used to reduce the amount of data passed between mappers and reducers.

### MapReduce Example: Word Count

The classic example of a MapReduce job is counting the words in a document.  Let's see how it fits with the MapReduce technique.

If we were going to count the words in a document on a single machine, we might follow a process like the following:

1. Read in a line of text
2. Split the line into words
3. Emit each word on its own line
4. Sort the words alphabetically
5. Count the sorted words.

Let's see how this would work in MapReduce.

* **Mapper**
  1. Read in a line of text
  2. Split the line into words
  3. Hash each word (mod number of reducers) to determine which reducer to send each word to.
* ** Shuffle (sort words per reducer) **
* ** Reducer **
  1. Reducer receives sorted words
  2. Count the sorted words.


We could also write this job with a combiner, decreasing network overhead at the cost of some slightly more complicated code.

* **Mapper**
  1. Read in a line of text
  2. Split the line into words
  3. Sort words and emit one word per line to the combiner
* **Combiner**
  1. Count sorted words and emit key-value pairs in the form of (word, count)
* ** Shuffle (sort words per reducer) **
* ** Reducer **
  1. Reducer receives sorted key-value pairs 
  2. Sum up the counts, reducing over the individual words.



### Taking advantage of MapReduce with `ni`
An important difference in philosophy between MapReduce and `ni` is how expensive sorting is; any MapReduce job you write will have the output of the mapper sorted (so long as your job has a reducer), so you always get (a ton of) sorting done for free. In `ni`, on the other hand, sorting is one of the most expenisve operations you can do because it requries buffering the entire stream to disk.

This makes clear a point that we introduced above in our discussion of containerized `ni` operations, and `ni` operations over `ssh`: one of the most powerful ways to use `ni` is to write what would otherwise be complicated scripts in `ni`'s concise and powerful syntax, and then push these scripts to platforms more suited to the task at hand using `ni` interops.

The key thing to remember for leveraging MapReduce's sort and shuffle with `ni` is the following:

> You can assume the output of each mapper and combiner, and the input of each combiner and reducer, is sorted.


### How `ni` Interacts with Hadoop Streaming MapReduce
When `ni HS...` is called, `ni` packages itself, its closures, **and its instructons** and submits itself as a mapper/combiner/reducer to the configured Hadoop server.

Because `ni` includes its data closures on submission, if these closures are too large, the Hadoop server will refuse the job.


### `HS[mapper] [combiner] [reducer]`: Hadoop Streaming MapReduce Job

`HS` creates a hadoop streaming job with a given mapper, combiner, and reducer (specified as `ni` operators). Any `ni` snippet can be used for the mapper, combiner, and reducer. 

Two shorthands for common Hadoop Streaming jobs exist:

* `_` skips the mapper/reducer/combiner. 
* `:` applies the trivial operation (i.e. redirect STDIN to STDOUT) for the mapper/reducer/combiner.

A useful Hadoop Streaming job that repartitions your data, for example, to be used in an HDFS join is the following:

`$ ni ... HS:_:`

It has a trivial map step, no combiner, and a trivial reducer; it looks like nothing is being done, but due to the shuffle in the MapReduce 

If the reducer step is skipped with `_`, the output may not be sorted, as one might expect from a Hadoop operation. Use `:` for the reducer to ensure that output is sorted correctly.



## Developing Hadoop Streaming Jobs

Because Hadoop is such an important part of `ni`'s power, we're devoting a section not just to the operator, but to the principles of development using `HS`. `HS` is, in fact actually a combination of two operations, `H` and `S`. `H` initiates a Hadoop Job, and `S` indicates that the job is a streaming job.

`ni` handles the creation of input and output paths for Hadoop, and the output of a Hadoop Streaming job is a path to the data where your data is stored, or more accurately, it is a `ni` command to read all the data from the output directory of 

### Fundamental Theorem of MapReduce

You can convert a hadoop streaming job to a `ni` job without Hadoop streaming via the following identity:

> `$ ni ... HS[mapper][combiner][reducer]` = `$ ni ... mapper gA combiner gA reducer`

This identity allows you to iterate fast, completely within `less` and the command line.
  
**Exercise**: Write a `ni` spell that counts the number of instances of each word in the `ni` source using Hadoop Streaming job. Start by writing the job for a single All of the tools needed for it (except the Hadoop cluster) are included in the first two chapters of this tutorial. Once you have it working, see how concise you can make your program.

### `HDS[mapper][combiner][reducer]`: Hadoop Develop Streaming (... coming soon)

I have this great idea to apply the fundamental theorem to `ni` jobs so the only thing you have to do is replace `HS` with `HDS` and you get a test-run of your job. It's been a little hard to cook up though.

## Architecting Hadoop Streaming Pipelines

Now that you have an idea of how to write a job that runs, it's important to think about how to write jobs that actually work. One of the most fun and frustrating things about Hadoop is that techniques that would solve a problem on a gigabyte of data and a single machine fail painfully when applied to a terabyte of data distributed over thousands of machines.

There is a big difference between jobs that can run and jobs that actually work in MapReduce, and you need to be aware of Hadoop's architecture and configuration to take advantage of it.

### MapReduce Pipelining Case Study

You are given the transactions from every Starbucks in the world, in the form of five columns: `Transasction ID, Store ID, Item ID, Date, Price`. You're instructed to identify the highest revenue item by store, by day.

On a gigabyte of data, this would be easy; you could use SQL or `pandas` to do something like:

```sh
day_store_item_df = 
	(raw_df.groupby(['Store ID', 'Date', 'Item ID'])['Price'].sum()
	 .reset_index().sort_values(['Price'], ascending=False)
	 .groupby(['Store ID', 'Date', 'Item ID'])
	 .first())
```

This idea doesn't translate over to MapReduce flawlessly. Let's take a look at one way to do this, using the store ID as the reduce key.

If we were to do that, all of the data from every store will go to the same reducer, and we'll have data on each reducer that is sorted by store. For simplicity, let's assume each store gets its own reducer.

We're first faced with the problem of one reducer having potentially a hundred times more data to process than another (if we use fewer stores than reducers, this problem persists in reduced form); this makes the job more prone to failure, as the longest-running reducers are also, in general, the most liable to fail or be pre-empted and killed by a scheduler.

Compounding this problem is that we need to do a secondary sort of the data on each reducer, to order the data by date and by item id.  You might also try to get around this sort by using a hash; this may work when the number of dates and item ids is smaller than several million, but when working with several terabytes of data, this number might grow to billions (consider the effect of getting data by hour or minute, rather than by day), and the combinatorial effects must always be considered.

A better approach is to first collect the data using a compound reduce key of `Store ID - Date - Item ID`. This will give us a good amount of randomization, approximately equal amounts of data per reducer, and a very easy and straightforward reduce step.

We likely want to collect the data by store, but running another MapReduce job using only `Store ID` as the reduce key may still be a bad idea, since there will still need to be a secondary sort of size on the order of `# of items x # of Days`. Instead, we can re-shard using `Store ID - Date` as the reduce key, and sort or hash something on the order of `# of items`.

At this point, we have the highest revenue item for each store and date; we may be able to stream the data out directly from HDFS, or run a trivial job to reshard the data by store.

### Single Step Down Principle

In each MapReduce pass, the whole dataset is swallowed, but it often can't be digested completely. When building a pipeline on a multi-terabyte dataset using MapReduce, I've found the following principle to be valuable: 

> Take a smaller step on a larger dataset rather than running the same larger task on multiple smaller datasets.

Some other ideas to keep in mind:

1. You must take advantage of randomization to run performant MapReduce.
1. Run as few separate jobs as possible.
1. Run the largest number of mappers and reducers that your cluster will allow, while maintaining a map time of at least 1 minute.
1. Your average reducer time should be under 5 minutes, if possible.
1. Your slowest reducer should take no more than twice as much time as your average reducer.
1. Be very careful of running a secondary sort in the reduce phase; often you're better off running a second job. 
1. Compress your data to the greatest extent possible.

### Hadoop Streaming MapReduce Limitations

#### Avoiding SIGPIPE

`ni`'s `r<number>` operator uses `head`, which is very fast; however, when `head` finishes, it sends a SIGPIPE signal; this signal will kill your Hadoop Streaming job if received before all of the data is consumed.

To remedy this, there is a "safe" version of `r<number>` called `rs<number>`, which has the exact same functions and specification as `r`, except that consumes the entire stream; it is significantly slower, but it will not fire off a SIGPIPE.

```bash
$ ni n1000 rs5
1
2
3
4
5
```

#### Number of Input Partfiles

It is hard-coded in Hadoop to not accept more than 100,000 input files as input for a single job. The error that you get if you try to use more than  this is not particularly informative, so try to keep this in mind.

#### Number of Mappers x Reducers

Mappers communicate their data to reducers over wired connections; the number of these connections that must be made is equal to the number of mappers times the number of reducers.

Because these connections are done over wire, they will fail with some frequency, and if the number of these failures gets too high, the entire `HS` job will fail.

In general, keep `(# of mappers) x (# of reducers) < 100 million` to avoid this issue.

#### More Notes on Hadoop Streaming
There are a number of Hadoop-specific issues that may make jobs that you can run on your machine not run on Hadoop. See the [optimization](optimization.md) docs or the [debugging](debugging.md) docs for more information.

### `^{...}`: `ni` configuration

As noted above, you need to take advantage of randomization to run successful MapReduce pipelines. Because our reducers receive sorted input, it's often the case that a Hadoop streaming job will fail as a result of too much data going to a single reducer. However, without instructions, `ni` will default to partitioning and sorting data to reducers using the first tab-delimited column. If this is not the default behavior you want, `ni` options can be set through environment variables in your `.bash_profile`. Setting ni configuration variables on the fly is often desirable, particularly in the context of hadoop operations, where increasing or decreasing the number of mappers and reducers, changing the way that data is partitioned and sorted 


```sh
$ ni ^{hadoop/partopt="-k1,1 -k2,2" \
       hadoop/nfields=3 \
       hadoop/sortopt="-k1,1nr -k3,3" \
  hadoop/jobconf='mapreduce.reduce.memory.mb=8192 mapreduce.job.reduces=128'} HS...
  
```

Some caveats about hadoop job configuration; Hadoop some times makes decisions about your job for you; often these are in complete disregard of what you demanded from Hadoop. When this happens, repartitioning your data may be helpful.

```sh
export NI_HADOOP_JOBCONF="mapreduce.job.reduces=1024"
```

Hadoop jobs are generally intelligent about where they spill their contents; if you want to change where in your HDFS this output goes, you can set the `NI_HDFS_TMPDIR` enviornment variable.

```sh
export NI_HDFS_TMPDIR=/user/my_name/tmp
```



### Reversible Hexadecimal to Base-64 Encoding `h2b64` and `b642h`

Hexadecimal is a very common form for storing ID data in a readable format. However, it is not highly compressed, as one hex character represents 4 bits of data while occupying an 8-bit printable character. We can save 1/3 of this data by converting it to base-64 which stores 6 bits of data in a larger alphabet of 8-bit printable characters. 

```bash
$ ni i0edd9c94-24d8-4a3e-b8fb-a33c37386ae1 p'h2b64 a'
Dt2clCTYSj64+6M8Nzhq4#
```

Decreasing the amount of data stored in each step will speed up every phase of your MapReduce pipeline and decrease your program's footprint. When you need the original data back, it can be returned with `b642h`.

```bash
$ ni i0edd9c94-24d8-4a3e-b8fb-a33c37386ae1 p'b642h h2b64 a'
0edd9c9424d84a3eb8fba33c37386ae1
```

If you really care about the dashes, they can be put back with `hyphenate_uuid`; this method only works with standard 32-character hexadecimal uuids.

```bash
$ ni i0edd9c94-24d8-4a3e-b8fb-a33c37386ae1 p'hyphenate_uuid b642h h2b64 a'
0edd9c94-24d8-4a3e-b8fb-a33c37386ae1
```

## HDFS I/O

### `hdfst://<path>` and `hdfs://<path>`: HDFS I/O

Hadoop Distributed File System (HDFS) is a redundant distributed file system that was based on a 2003 [paper](https://static.googleusercontent.com/media/research.google.com/en//archive/gfs-sosp2003.pdf) by Sanjay Ghemawat, Howard Gobioff, and Shun-Tak Leung of Google.

HDFS stores files in triplicate, and distributes copies of files across different nodes in a cluster. This helps prevent data loss in the event of hard drive failure, and allows for MapReduce jobs to use data-local processing, which decreases network overhead (which is often rate-limiting in Hadoop MapReduce jobs).

There are two `ni` operators 

`$ ni hdfs://<abspath> ...`

This is equivalent to `$ hadoop fs -cat <abspath> | ni ...`

`$ ni hdfst://<abspath> ...`

This is equivalent to `$ hadoop fs -text <abspath> | ni ...`

Files are often stored in compressed form on HDFS, so `hdfst` is usually the operator you want. 

Also, note that the paths for the HDFS I/O operators must be absolute; thus HDFS I/O operators start with **three** slashes, for example: `$ ni hdfst:///user/bilow/data ...`


### Using HDFS paths in Hadoop Streaming Jobs

If you want to use data in HDFS for Hadoop Streaming jobs, you need to use the path as literal text, which uses the `i` operator (the literal text operator from Chapter 1)

```sh
$ ni ihdfst://<abspath> HS...
```

This will pass the directory path directly to the Hadoop Streaming job, which is the behavior you want (in general). 

If you do not use path, as in:

```sh
$ ni hdfst://<abspath> HS...
```

`ni` will read all of the data out of HDFS to the machine from which ni is being called, stream that data to an HDFS temp-path, and run the Hadoop job using the temp folder. That's a huge amount of overhead compared to just quoting the path.  If you run the code on a quoted path, your Hadoop Streaming job should start in under 3 minutes. If you don't quote the path, it might take hours. Quote the damn path.



## `nfu` HDFS Joins

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

You can appropriately process the checkpoint files to to get the correct paths, then use:

```
nfu hdfs://<abspath1> -j [hdfs://<abspath> 0] _
```

`nfu` offers two types of joins, inner and left-outer, and two options for whether the inputs need to be sorted. 

* `i`: Inner join with preceding sort
* `j`: Inner join without sort
* `I`: Left-outer join with preceding sort
* `J`: Left-outer join without sort


## Conclusion

The classic word count program for Hadoop can be written like this:

>`$ ni //ni HS[FWpF_] _ [c]`

If you've never had the opportunity to write the word count MapReduce program in another language, take a look at the state of the art in:

* [Python](http://www.michael-noll.com/tutorials/writing-an-hadoop-mapreduce-program-in-python/)
* [Ruby](http://www.bigfastblog.com/map-reduce-with-ruby-using-hadoop)
* [Java](https://hadoop.apache.org/docs/stable/hadoop-mapreduce-client/hadoop-mapreduce-client-core/MapReduceTutorial.html)
* [Go](http://go-wise.blogspot.com/2011/09/go-on-hadoop.html)
* [Perl](http://www.perlmonks.org/?node_id=859535)

It would it take me at least an hour to get through the tutorial in the language I know best (Python). Look at how convoluted the Java program is; can you imagine trying to explain what it actually means to someone who isn't already highly skilled in the language?

What's more important is that all of the examples above are completely uninspired, joyless programs. Every single one of those programs makes make me hate programming.

`ni` does the opposite. I wrote the `ni` spell in about 5 seconds, and I can explain how it works in about 30. Even at this early stage, I bet it didn't take more than a couple of minutes to figure out how to write the program either. It's easily tested, readable, and concise, and beautiful. You should be excited about the possibilities just over the horizon.

Congrats on finishing this chapter of the tutorial. In the first two chapters, you've been introduced tools for manipulating and expanding individual rows of data; in the next chapter we'll develop tools that condense and combine multiple rows of data into one. We'll also look at some specialized `ni` functions, and `ni` interoperability with Ruby, Lisp, and Python/numpy.

	#`ni` by Example, Chapter 2 (alpha release)

Welcome to the second part of the tutorial. At this point, you should be familiar with fundamental row and column operations; sorting; basic Perl operations; file I/O and comopression; and some basic principles of `ni` style. Before continuing, it's advisable to read over the first part of the horribly misnamed [cheatsheet](cheatsheet.md) to see some of the operations that were glossed over.

The key concept that we will cover (and really, the key to `ni`'s power) is the ability of `ni` to package itself and execute in-memory on remote machines. To that end, we will explain the use of `ni` on local Docker instances; over `ssh` on remote machines, and how to use `ni` to write simple and powerful Hadoop Streaming jobs. 

Other key concepts for this tutorial include streaming reduce operations, data closures, and cell operations. We'll also cover more `ni`-specific Perl extensions, and some important parts of Perl that will be particularly useful in `ni`.

Before we get into anything too useful, however, we need to take a detour into how `ni` works at a high level. It's not completely necessary to know this in order to use `ni`, but understanding this will help you think like `ni`. 

##`ni` is self-modifying

`ni` is written in [self-modifying Perl](https://github.com/spencertipping/writing-self-modifying-perl), and the ability to rewrite its source code is the key to its virality. In biological terms, it is useful to think of `ni` is truly viral; it can be run in-memory on any machine with bash and Perl.


###`ni` evaluation basics
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

###`::closure_name[...]`: Create a data closure

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


Data closures provide a counterexample to the basics of `ni` evaluation written above. 

```bash
$ ni ::five[n5] | ni n3p'r a, five'
1	five
2	five
3	five
```

The reason that the example including pipes gives different results than the example with no pipes is that **creating the data closure modifies `ni` itself**.  In the piped example, the first `ni` is modified but is not used; the second `ni` is identical to the first `ni` before the data closure was called into existence, so it cannot access the data closure built in the first `ni`.

###Perl Bareword Interpretation
The piped example above bears a second look for the reason that it returns output rather than raising an error, even though the data closure `five` is not in its namespace.

```bash
$ ni ::five[n5] | ni n3p'r a, five'
1	five
2	five
3	five
```

The Perl interpreter will convert missing barewords (i.e. things that do not start with a Perl sigil [`$, @, %`, etc.]) as a string. This trick is useful for writing strings without spaces within Perl environments; most of them do not need to be quoted. It is good `ni` style to avoid using quotes when they are unnecessary.

We now return to our regularly-scheduled programming.

###Data Closure Evaluation and `ni` self-modification
Data closures, regardless of where they are written in a ni spell, will be evaluated before everything else, and in the order that they are written.

That means that `$ ni ::five[n5] n3p'r a, five'` ie equivalent to `$ ni n3p'r a, five' ::five[n5]`, even though in the latter, it looks like it's referencing something that was computed later in the pipeline.

Data closures are (or will be shortly implemented such that they are) computed in order; that means you can use one data closure to compute another, so long as the one computed from is expressed in the stream before the one which uses it to do some computation.

We can rewrite a `ni` pipeline a little more accuartely as the following:

```
$ ni ::dataclosure1 ... ::dataclosureM <op1> <op2> ... <opN> 
```

```
$ ni ::dataclosure1 ... ::dataclosureM (export to) ni_prime
$ ni_prime op1 | ni_prime op2 | ... | ni_prime opN
```

We will see how `ni` actually does this in the following sections.


##`ni` is a quine

A _quine_ (pronounced: KWINE) is a program that prints its source code when it is run. If you haven't run into quines before (or the equivalent terms selfrep or self-representing program), and you go out and start looking at them, they can be mind-bending and near-impossible to read. That is the correct reaction; you should start becoming comfortable with that feeling.

We'll write a classic quine in Scheme (or Lisp), then a quine in Perl, and then demonstrate that `ni` is a quine without getting too deep into the details.

###Scheme/Lisp mini-tutorial
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
  * `(lambda (u v) (u + v))` yields a lambda (i.e. an anonymous function) that adds two values.
  * `((lambda (u v) (u + v)) 4 5)` yields 9, because the 4 and the 5 are passed as arguments to the lambda.

###A simple quine in Scheme
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

For those inclined to theoretical computer science, [David Madore's tutorial on quines](http://www.madore.org/~david/computers/quine.html) is excellent.  For more examples quines, see [Gary P. Thompson's page](http://www.nyx.net/~gthompso/quine.htm)

###A quine in Perl

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

```
#!/usr/bin/perl
eval($_=<<'_');
print 
```
and data:

```
"#!/usr/bin/perl\neval(\$_=<<'_');\n${_}_\n"
_
```

What makes this quine a bit more difficult to read is that there is some overlap between the code and the data involving the assignment statement. Also, Perl.


Copying the lines and running: 

```
$ pbpaste > quine.pl
$ cat quine.pl | perl
#!/usr/bin/perl
eval($_=<<'_');
print "#!/usr/bin/perl\neval(\$_=<<'_');\n${_}_\n"
_
```

Most of the trickery is done using the heredoc syntax (which is a way of writing multi-line strings starting with ). Heredocs are parsed in a non-obvious way. In this case, at a high level, the code works like this:

1. `$_` becomes the code written as a heredoc, which is just the 3rd line. The heredoc parser knows to skip the end of the line.
2. `eval $_` happens, causing that code to be executed
3. inside `eval $_`, `print "#!... ${_}..."` happens -- notice the reference back to `$_`, which contains the code being evaled

The key here is that because the code is inside a single-quoted heredoc, it can be interpolated directly into its own representation.

###Quines, so what?

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



###`//ni`: Print `ni`'s _current_ source
As a reminder, you should be using a vanilla (to the greatest extent possible) bash shell for these commands. If you're not running a bash shell, `bash` at the terminal will pop you into a bash shell.


`ni` is a quine. To get `ni` to print its source, run:

`$ ni //ni`

```
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


##`ni` is a self-modifying quine

In the section on `ni` being self-modifying, it was mentioned that `ni` execution follows a structure something like this:

```
$ ni ::dataclosure1 ... ::dataclosureM <op1> <op2> ... <opN> 
```

is equivalent to

```
$ ni ::dataclosure1 ... ::dataclosureM (export to) ni_prime
$ ni_prime op1 | ni_prime op2 | ... | ni_prime opN
```

In fact, this `ni_prime` is a local modification of `ni`, which incorporates data closures. The details are outside the scope of how this occurs are outside the scope of this tutorial, but here's some evidence that this is going on.

```
$ ni //ni | wc -c
  743514
```

```
$ ni ::my_closure[n10] //ni | wc -c
  743569
```

If we've really inserted a data closure into `ni` as a quine, `ni` is really a quine, then we should be able to execute it, for example, by passing the code to perl.

```
$ ni ::five[n5] //ni | perl - n1p'five'
1
2
3
4
5
```

This is really quite magical; we've taken `ni`, made a simple but powerful modification to its source, then passed the entire source to `perl` (which had no idea what it would receive), and it was able to access something that doesn't exist in the installed version of `ni`:

```bash
$ ni //ni | perl - n1p'ten'
ten
```

One final note; by the ordering of the data, it may appear that the fact that `ni` is self-modifying and the fact that it is a quine are separate; or that the self-modifying power of `ni` makes it a quine. In fact, the opposite is true; it is because `ni` is a quine that allows it to be self-modifying.

##SSH, Containers, and Horizontal Scaling

We've covered why `ni` can be indirectly executed on the same machine using the identity `$ ni ...` == `$ ni //ni | perl - ...`. The natural next steps are to explore indirect execution of `ni` scripts on virtual machines and on machines you control via `ssh`. While horizontal scaling (running a process on multiple cores) has nothing to do with the indirect execution in containerized operations, it has functional and semantic similarity with these other operators in this section.

###`C`: execute in a container

Running in containers requires that Docker be installed on your machine. It is easy to install from [here](https://www.docker.com/).

Running containers can be especially useful to take advantage of better OS-dependent utilities. For example, Mac OS X's `sort` is painfully slow compared to Ubuntu's. If you are developing on a Mac, there will be a noticeable performance change using `$ ni n1E7 g` vs `$ ni n1E7 Cubuntu[g]`.


###`s`: execute over `ssh`

You will need to set up your hosts properly in your `.ssh/config` to use a named host. For example, if you log in with the command `ssh user.name@host.name:port.number`, you would create an alias for that host by entering the following lines in your `.ssh/config` 

```
host <alias>
    HostName <host.name>
    Port <port.number>
    User <user.name>
```

You would access this as `$ ni ... s<alias>[...]`. The alias used in most of the other `ni` docs is `dev`, for your primary work machien.

Inside the brackets,  you will have access to the filesystem of the remote machine (but not the machine from which `ni` was originally called). 

###`S`: Horizontal Scaling 
Remember that `ni` should be I/O bounded; as such, `ni` provides a very easy interface to multiprocessing using the horizontal scaling operator, `S`. 

`$ ni S<# cores>[...]`: distributes the computation of `...` across `<# cores>` processors. 

Running an operator with `S8` on a machine with only two cores is not going to give 8x the computing power. In fact, if you request more cores than you have, you'll likely end up slowing your progress.  


  
##Hadoop Streaming MapReduce

`ni` and MapReduce complement each other very well; in particular, the MapReduce paradigm provides efficient large-scale sorting and massive horizontal scaling to `ni`, while `ni` provides concise options to 

###MapReduce Fundamentals

MapReduce landed with a splash in 2004 with this [excellent paper](https://static.googleusercontent.com/media/research.google.com/en//archive/mapreduce-osdi04.pdf) by Jeffrey Dean and Sanjay Ghemawat of Google and (with Hadoop) in many ways ushered in the era of "big data."

To understand how to best use `ni` with MapReduce, it's important to understand how it works.

The MapReduce paradigm breaks down into three steps:

1. Map
2. Combine (optional)
3. Reduce

In general, a mapper will read in large, often unstructured data, and output  more highly structured information, which will be combined into statements about the original data by the reduce operation.  Because both map and reduce occur across many processors, there is often high network overhead transferring data from mappers to reducers. A combiner is used to reduce the amount of data passed between mappers and reducers.

###MapReduce Example: Word Count

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



###Taking advantage of MapReduce with `ni`
An important difference in philosophy between MapReduce and `ni` is how expensive sorting is; any MapReduce job you write will have the output of the mapper sorted (so long as your job has a reducer), so you always get (a ton of) sorting done for free. In `ni`, on the other hand, sorting is one of the most expenisve operations you can do because it requries buffering the entire stream to disk.

This makes clear a point that we introduced above in our discussion of containerized `ni` operations, and `ni` operations over `ssh`: one of the most powerful ways to use `ni` is to write what would otherwise be complicated scripts in `ni`'s concise and powerful syntax, and then push these scripts to platforms more suited to the task at hand using `ni` interops.

The key thing to remember for leveraging MapReduce's sort and shuffle with `ni` is the following:

> You can assume the output of each mapper and combiner, and the input of each combiner and reducer, is sorted.


###How `ni` Interacts with Hadoop Streaming MapReduce
When `ni HS...` is called, `ni` packages itself as a `.jar` to the configured Hadoop server, which includes all the instructions for Hadoop to run `ni`.

Remember that when `ni` uploads itself, it uploads the self-modified version of itself including all data closures. If these closures are too large, the Hadoop server will refuse the job.


###`HS[mapper] [combiner] [reducer]`: Hadoop Streaming MapReduce Job

`HS` creates a hadoop streaming job with a given mapper, combiner, and reducer (specified as `ni` operators). Any `ni` snippet can be used for the mapper, combiner, and reducer. 

Two shorthands for common Hadoop Streaming jobs exist:

* `_` skips the mapper/reducer/combiner. 
* `:` applies the trivial operation (i.e. redirect STDIN to STDOUT) for the mapper/reducer/combiner.

A useful Hadoop Streaming job that repartitions your data, for example, to be used in an HDFS join is the following:

`$ ni ... HS:_:`

It has a trivial map step, no combiner, and a trivial reducer; it looks like nothing is being done, but due to the shuffle in the MapReduce 

If the reducer step is skipped with `_`, the output may not be sorted, as one might expect from a Hadoop operation. Use `:` for the reducer to ensure that output is sorted correctly.

###Initializing `HS` Jobs
`HS` takes data in several formats. You can stream 


##Developing Hadoop Streaming Jobs

Because Hadoop is such an important part of `ni`'s power, we're devoting a section not just to the operator, but to the principles of development using `HS`. `HS` is, in fact actually a combination of two operations, `H` and `S`. `H` initiates a Hadoop Job, and `S` indicates that the job is a streaming job.

outputs the name of the directory where the output has been placed.

`ni` handles the creation of input and output paths for Hadoop, and the output of a Hadoop Streaming job is a path to the data where your data is stored.


Since the output of the Hadoop Streaming job is a directory, the To read data from the output of a Hadoop Streaming job

You can convert a hadoop streaming job to a `ni` job without Hadoop streaming via the following identity:

> `$ ni ... HS[mapper][combiner][reducer]` = `$ ni ... mapper gA combiner gA reducer`

This identity allows you to iterate fast, completely within `less` and the command line.
  
**Exercise**: Write a `ni` spell that counts the number of instances of each word in the `ni` source using Hadoop Streaming job. Start by writing the job for a single All of the tools needed for it (except the Hadoop cluster) are included in the first two chapters of this tutorial. Once you have it working, see how concise you can make your program.

###`HDS[mapper][combiner][reducer]`: Hadoop Develop Streaming


###`:checkpoint_name[...]`: Checkpoints

Developing and deploying to producion `ni` pipelines that involve multiple Hadoop streaming steps involves a risk of failure outside the programmer's control; when this happens, we don't want to have to run the entire job again. 

Checkpoints allow you to save the results of difficult jobs while continuing to develop in `ni`.

Checkpoints and files share many commonalities. The key difference between a checkpoint and a file are:

1. A checkpoint will wait for an `EOF` before writing to the specified checkpoint name, so the data in a checkpoint file will consist of all the output of the operator it encloses. A file, on the other hand, will accept all output that is streamed in, with no guarantee that it is complete.
2. If you run a `ni` pipeline with checkpoints, and there are files that correspond to the checkpoint names in the directory from which `ni` is being run, then `ni` will use the data in the checkpoints in place of running the job. This allows you to save the work of long and expensive jobs, and to run these jobs multiple times without worrying that bhe steps earlier in the pipeline that have succeeded will have to be re-run.

An example of checkpoint use is the following:

```bash
$ ni nE6gr4 :numbers
1
10
100
1000
```

This computation will take a while, because `g` requires buffering to disk; However, this result has been checkpointed, thus more instuctions can be added to the command without requiring a complete re-run.


```bash
$ ni nE6gr4 :numbers O
1000
100
10
1
```

Compare this to the same code writing to a file:

```
$ ni nE6gr4 =\>numbers
1000
100
10
1
```

Because the sort is not checkpointed, adding to the command is not performant, and everything must be recomputed.

```
$ ni nE6gr4 =\>numbers O
1000
100
10
1
```

One caveat with checkpoint files is that they are persisted, so these files must be cleared between separate runs of `ni` pipelines to avoid collisions. Luckily, if you're using checkpoints to do your data science, errors like these will come out fast and should be obvious.


###Developing Hadoop Streaming pipelines with checkpoints

As of now, `ni` auto-generates the names for the Hadoop directories, and these can be hard to remember off the top of your head.

###A Final Note on Hadoop Streaming Jobs
There are a number of Haddop-specific issues that may make jobs that you can run on your machine not run on Hadoop. See the [optimization](optimization.md) docs or the [debugging](debugging.md) docs for more information.


##HDFS I/O

###`hdfst://<path>` and `hdfs://<path>`: HDFS I/O

Hadoop Distributed File System (HDFS) is a redundant distributed file system that was based on a 2003 [paper](https://static.googleusercontent.com/media/research.google.com/en//archive/gfs-sosp2003.pdf) by Sanjay Ghemawat, Howard Gobioff, and Shun-Tak Leung of Google.

HDFS stores files in triplicate, and distributes copies of files across different nodes in a cluster. This helps prevent data loss in the event of hard drive failure, and allows for MapReduce jobs to use data-local processing, which decreases network overhead (which is often rate-limiting in Hadoop MapReduce jobs).

There are two `ni` operators 

`$ ni hdfs://<abspath> ...`

This is equivalent to `$ hadoop fs -cat <abspath> | ni ...`

`$ ni hdfst://<abspath> ...`

This is equivalent to `$ hadoop fs -text <abspath> | ni ...`

Files are often stored in compressed form on HDFS, so `hdfst` is usually the operator you want. 

Also, note that the paths for the HDFS I/O operators must be absolute; thus HDFS I/O operators start with **three** slashes, for example: `$ ni hdfst:///user/bilow/data ...`


###Using HDFS paths in Hadoop Streaming Jobs:

If you want to use data in HDFS for Hadoop Streaming jobs, you need to use the path as literal text, which uses the `i` operator (explained in more detail below)

```
$ ni ihdfst://<abspath> HS...
```

This will pass the directory path directly to the Hadoop Streaming job. If you do not use path, as in:

```
$ ni hdfst://<abspath> HS...
```

When the input path is not quoted, `ni` will read all of the data out of HDFS to the machine from which ni is being called, stream that data to an HDFS temp-path, and run the Hadoop job using the temp folder (and clean up the temp-path). That's a huge amount of overhead compared to just quoting the path.  If you run the code on a quoted path, your Hadoop Streaming job should start in under 3 minutes. If you don't quote the path, it might take hours. Quote the damn path.


###`i`: Literal text 
To introduce how to use how to use HDFS with `HS` we need to introduce another more fundamental `ni` operator, for reasons that will be clear later. `i` operator is the way to put literal text into the command line:

```bash
$ ni ihello ithere
hello
there
```

You can use single quotes with `i` to include spaces within strings.

```bash
$ ni i'one whole line'
one whole line
```

If you want your text to be tab-delimited, you can put your text inside brackets.

```bash
$ ni i[foo bar]
foo	bar
```

And if you need brackets in your text, you can put those brackets inside brackets (and add spaces around the beginning and ending brackets.)


```bash
$ ni i[ foo[] [bar] ]
foo[]	[bar]
```


###`^{...}`: `ni` configuration

You can set `ni` options through environment variables in your `.bash_profile`. Setting ni configuration variables on the fly is sometimes desirable, particularly in the context of hadoop operations, where increasing or decreasing the number of mappers and reducers (controlled by ni configs) may have significant performance benefits.


```
$ ni ^{hadoop/name=/usr/local/hadoop/bin/hadoop \
  hadoop/jobconf='mapred.map.tasks=10 \
  mapred.reduce.tasks=4'} HS...
  
```

Some caveats about hadoop job configuration; Hadoop some times makes decisions about your job for you; often these are in complete disregard of what you demanded from Hadoop. When this happens, repartitioning your data may be helpful; I believe the job server has to provide you at least 1 mapper for each of the input splits.

```
export NI_HADOOP_JOBCONF="mapreduce.job.reduces=1024"
```

Hadoop jobs are generally intelligent about where they spill their contents; if you want to change where in your HDFS this output goes, you can set the `NI_HDFS_TMPDIR` enviornment variable.

```
export NI_HDFS_TMPDIR=/user/bilow/tmp
```


##Conclusion

The classic word count program for Hadoop can be written like this.

>`$ ni //ni HS[FWpF_] _ [c]`

If you've never had the opportunity to write the word count MapReduce program in another language, take a look at the state of the art in:

* [Python](http://www.michael-noll.com/tutorials/writing-an-hadoop-mapreduce-program-in-python/)
* [Ruby](http://www.bigfastblog.com/map-reduce-with-ruby-using-hadoop)
* [Java](https://hadoop.apache.org/docs/stable/hadoop-mapreduce-client/hadoop-mapreduce-client-core/MapReduceTutorial.html)
* [Go](http://go-wise.blogspot.com/2011/09/go-on-hadoop.html)
* [Perl](http://www.perlmonks.org/?node_id=859535)

It would it take me at least an hour to get through the tutorial in the language I know best (Python). Look at how convoluted the Java program is; can you imagine trying to explain what it actually means to someone who isn't already highly skilled in the language?

What's more important is that all of the examples above are completely uninspired, joyless programs. Every single one of those programs makes make me hate programming.

`ni` does the opposite. I wrote the `ni` spell in about 5 seconds, and I can explain how it works in about 30. Even at this early stage, I bet it didn't take more than a couple of minutes to figure out how to write the program either. It's easily tested by dro, readable, concise, and beautiful. You should be excited about the possibilities just over the horizon.

Congrats on finishing this chapter of the tutorial. In the first two chapters, you've been introduced tools for manipulating and expanding individual rows of data; in the next chapter we'll develop tools that condense and combine multiple rows of data into one. We'll also look at some specialized `ni` functions, and `ni` interoperability with Ruby, Lisp, and Python/numpy.
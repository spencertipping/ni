#`ni` by Example, Chapter 1 (alpha release)

Welcome! This is a "rich" `ni` tutorial that covers all of the basics of this cantankerous, odd, and ultimately, incredibly fast, joyful, and productive tool. We have tried to assume as little knowledge as possible in this tutorial, but if you find anything confusing, please contact [the developers](http://github.com/spencertipping) or [the author](http://github.com/michaelbilow).

By the end of this tutorial, you should have a good handle on the most common `ni` operations, and should be pretty facile with `ni` on your local machine. `ni` suffers from having both a steep learning curve and an exponential power curve; learning the basics of the syntax is the hardest part, and knowing the basics won't make you much more productive (especially if you have a couple of years of Python under your belt and know another data processing language like `pandas`).

The knowledge represented in this tutorial is fundamental, but only scratches the surface of `ni`'s power, which is its virality. This is covered in depth in [Part 2](ni_by_example_2.md).  Stick with it, and you'll be rewarded (we promise).

In general, this tutorial follows along with the horribly-misnamed `ni` [cheatsheet](cheatsheet.md). If you find this tutorial too slow, you can drink from the firehose there. Have fun!

##What is `ni`?

`ni` is write-anywhere, run-everywhere

`ni` is a self-modifying quine.

`ni` is everything-from-the-command-line.

`ni` is what you need from Flink and Spark and Luigi 

`ni` is concise.

`ni` is beautiful.

`ni` is two-fisted data science.


##Installation
`ni` should work on any Unix-based OS. If you have Windows and want `ni`, go get Cygwin or VirtualBox or Docker or save yourself the trouble and give your hard drive a good wipe and a fresh Ubuntu install. 

```
git clone git@github.com:spencertipping/ni.git
cd ni
./build
ln -s $PWD/ni ~/bin/ni  # or whatever to add it to your path
```

##`ni` Development Environment

It's **highly** recommended to run `ni` from a `bash` prompt, and ideally one that is as vanilla as possible; if you're using some other CLI and have `bash` installed, `bash` at the command line will open a bash shell (using your `~/.bash_profile` settings)


##Integer Streams
`$ ni n10` will drop you into a screen that looks like this:

```
1
2
3
4
5
6
7
8
9
10
(END)
```

If you're familiar with the Unix terminal pager utility `less`, this will look and feel familiar. If you're not, `q` will quit and return to the command line. 

####`n`: Integer Stream
`ni n` generates a stream of consecutive integers starting at 1. The number after determines how many numbers will be generated.

Without an argument, `ni n` gives an infinite stream of consecutive integers starting from 1.

`ni n0` gives you consecutive integers starting from zero.

To generate a large but finite number of integers, you can use scientific notation with `n`. `ni n3.2E5` will give you 3.2 x 10<sup>5</sup> consecutive integers, starting from 1.


##File Output
`$ ni n10 \>ten.txt`

Running this command will drop you into `less` with this as output:

```
ten.txt
(END)
```

The directory you ran this command from, you should have a file called `ten.txt`.  If you open `ten.txt` in your text editor of choice, you should find the integers from 1 to 10, each printed on its own line.


####`\>`: Output and Emit File Name

`ni ... \>ten.txt` outputs the stream to a file called `ten.txt` and emits the file name in a `less` pager.

Note that there is **no space** between `\>` and `ten.txt`. Because `ni` is concise, whitespace is frequently important.

##File Input
`$ ni ten.txt ten.txt`

If you don't have the `ten.txt` file in your current directory, run `$ ni n10 \>ten.txt`.

This should yield:

```
1
2
3
4
5
6
7
8
9
10
1
2
3
4
5
6
7
8
9
10
(END)
```

Entering the name of a file at the command line will cause `ni` to `cat` the contents of the file into the stream. If more than one file name is entered, they will be added to the stream in order. `ni` also automatically decompresses most common formats, including gzip, bzip, xzip, lzo, and lz4.

**BUG**: `ni` will let you name your files the same name as `ni` commands. should a warning be raised?


`ni` will look for filenames before it uses its own operators. Therefore, be careful not to name your files anything too obviously terrible. For example,

```
$ ni n5 \>n10
$ ni n10
```

yields:

```
1
2
3
4
5
(END)
```
because it is reading from the file named `n10`.


##`ni` Coding and Debugging

The simplest way to build up a `ni` spell is by writing one step of the spell, checking that step's output for correctness, then writing another step.

In general, `ni` spells will start producing output very quickly (or can be coerced to produce output quickly). Once the output of one step in the spell looks good, you can move on to the next step.

As you advance through this tutorial, or start working with `ni` spells written by others, you'll want a quicker way to understand at a high level what a particular `ni` spell is doing.

For this, use `ni --explain ...`. Using the example from the file output section:

```
$ ni --explain n10 \>ten.txt
["n",1,11]
["file_write","ten.txt"]
```

Each line represents one step of the pipeline defined by the spell, and the explanation shows how the `ni` parsleyer interprets the 

From now on, we'll use `ni --explain ...` to start the analysis of each of the spells we write. 


##Stream Duplication and Compression
`$ ni n10 =[\>ten.txt] z\>ten.gz`

Running the spell puts us back in `less`:

```
ten.gz
(END)
```

The last statement is another `\>`, which, as we saw above, writes to a file and emits the file name. That checks out with the output above.

To examine the contents 

Let's take a look at this with `--explain`:

```
$ ni --explain n10 =[\>ten.txt] z\>ten.gz
["n",1,11]
["divert",["file_write","ten.txt"]]
["sh","gzip"]
["file_write","ten.gz"]
```

We recognize the first and last operators instantly; the middle two operators are new.

####`=[...]`: Divert Stream

Looking at the output of `ni --explain`:

```
...
["divert",["file_write","ten.txt"]]
...
```

We see that, after `"divert"`, the output of `ni --explain` of the operator within brackets is shown as a list.

`=` is formed of two parallel lines, this may be a useful mnemonic to remember how this operator functions; it takes the input stream and duplicates it.  One copy is diverted to the operators within the brackets, while the other copy continues to the other operations in the spell.

One of the most obvious uses of the `=` operator is to sink data to a file midway through the stream while allowing the stream to continue to flow.

For simple operations, the square brackets are unnecessary; we could have equivalently written:

`ni n10 =\>ten.txt z\>ten.gz`

This more aesthetically-pleasing statement is the preferred `ni` style. The lack of whitespace between `=` and the file write is critical.

####`z`: Compression

Compression is fundamental to improving throughput in networked computation, and `ni` provides a keystroke-efficient interface. As `--explain` says, the compression used is `gzip`, and it is called through the shell. `z` takes a lot of different options, which you can read about in the [cheatsheet](cheatsheet.md). 

`ni` decompresses its input by default. We can take our output and look at it in `ni` very easily using `$ ni ten.gz` or `$ cat ten.gz | ni`, which returns:

```
1
2
3
4
5
6
7
8
9
10
(END)
```

as we would expect.



##Basic Row Operations, File Reading, and Output Redirection
`$ ni n10 =z\>ten.gz r3 \>three.txt \< | wc -l`

Running the spell, we are not dropped into a `less` environment; the output has been successfully piped to `wc -l`, and the lines of the stream have been successfully counted.

```
$ ni n10 =z\>ten.gz r3 \>three.txt \< | wc -l
      3
```

Because of the pipe, you cannot simply run `$ ni --explain n10 =z\>ten.gz r3 \>tens.txt \< | wc -l`, which will pipe the output of `ni --explain` to `wc -l` and count the number of lines in the explanation. Dropping the part after the pipe yields:

```
$ ni --explain n10 =z\>ten.gz r3 \>tens.txt \<
["n",1,11]
["divert",["sh","gzip"],["file_write","ten.gz"]]
["head","-n",3]
["file_write","tens.txt"]
["file_read"]
```

A wrinkle has been added into the `divert` statement, demonstrating the ability to do more complicated operation of compressing and writing to a file. The lack of whitespace here is critical, and this more concise command is preferred stylistically to the explicit and functionally equivalent `=[ z \>ten.gz ]`.


####`r`: Take Rows

`r` is a powerful and flexible operation for filtering rows. Here's a short demonstration of its abilities. Eventually you will have all of these operations memorized, but for now, just try to remember that you have these options available to you.

* `$ ni n10 r3` - take the first 3 rows of the stream
* `$ ni n10 r-3` - take everything after the first 3 rows of the stream
* `$ ni n10 r~3` - take the last 3 rows of the stream
* `$ ni n10 rx3` - take every 3rd row in the stream
* `$ ni n10 r.15` - sample 15% of the rows in the stream
  * The sampling here is deterministic (conditioned on the environment variable `NI_SEED`) and will always return the first row.

`r` also can take a regex: `$ ni <data> r/<regex>/` takes all rows where the regex has a match.

For example, you can take all of the numbers that end in 0, 1, or 5 with the following spell: `$ ni n20 r/[015]$/`

Because you're typing directly into the `bash` shell, some characters may need to be escaped, for example in this expression which identifies numbers that are all repeating digits.

`$ ni n1000 r/^\(\\d\)\\1+$/`

Use of escape characters in `ni` operators is acceptable style only if there is not a conciser way that is at least as readable, or a more readable way that is at least as concise. way to do the job. In this case, there is both. Making use of the Perl operator, introduced in the next section allows the above spell to be written more clearly and concisely written as `ni n1000 rp'/^(\d)\1+$/'`

The `r` operator is especially useful during development; for example, if you are working with a large file or stream, you can check the correctness of your output using `r10`, `rx100`, `r.001` etc. to downsample and cap the amount of data read.  

> With the exception of operators that require processing the entire stream (sorting, for example) all `ni` development can be I/O-bounded, and not processor-bounded, regardless of the resources required by the computation.


####`\<`: Read from File Names in Stream
When the `\>` file writing operator was introduced, you may have questioned what the purpose of emitting the filename was. The answer is the file reading operator `\<`.

`\<` interprets its input stream as file names, and it will output the contents of these files in order. 

Note that `ni` does not insert newlines between input from separate files. In general, this is a feature, as it allows zip files to be partitioned (for example, in HDFS). However, if you're reading from multiple raw text files, you may need to make sure that the file ends with a newline.

####`|` and `>`: Piping and Redirecting Output

Like other command-line utilities, `ni` respects pipes (`|`) and redirects (`>`). 


##Perl Operations
`ni` and Perl (5) go well together philosophically. Both have deeply flawed lexicons and both are highly efficient in terms of developer time and processing time. `ni` and Perl scripts are both difficult to read to the uninitiated. They demand a much higher baseline level of expertise to understand what's going on than, for example, a Python script. 

In addition to Perl, `ni` offers direct interfaces to Ruby and Lisp. While all three of the languages are useful and actively maintained, `ni` is written in Perl, and it is by far the most useful of the three. If you haven't learned Perl yet, but you're familiar with another scripting language, like Python or Ruby, I found [this course](https://www.udemy.com/perltutorial/learn/v4/) on Udemy useful for learning Perl's odd syntax.

We'll start with the following `ni` spell.

`$ ni /usr/share/dict/words rx40 r10 p'r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)'`

The output here will depend on the contents of your `/usr/share/dict/words/`, but you should have 3 columns; the first 3 letters of each word, the second 3 letters of each word, and any remaining letters. On my machine it looks like this:

```
aba     iss     ed
aba     sta     rdize
abb     rev     iature
abd     uct     ion
abe     tme     nt
abi     gai     l
abj     udg     e
abl     est     
abo     lis     h
abo     rti     vely
(END)
```

Notice that `ni` has produced tab-delimited columns for us; these will be useful for the powerful column operators we will introduce in this section and the next.

```
$ ni --explain /usr/share/dict/words rx40 r10 p'r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)'
["cat","/usr/share/dict/words"]
["row_every",40]
["head","-n",10]
["perl_mapper","r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)"]
```

We have reviewed every operator previously except the last. 


####`p'...'`: Map Perl over rows
When you think of writing a simple data processing program in Python, Ruby, C, or even Perl, think about how many keystrokes are spent loading libraries that are used mostly implicitly; and if they're not loaded, the program won't run (or, it will run, but it's bad style, or it's hard to test, or it's okay for a small script, but it's too hard to read, or it's _dangerous_).

Even the act of writing a script that reads from standard input and writes to standard output, maybe compiling it, and then calling it with arguments from the command line requires a lot of task-switching.  

`ni` removes all of that; the moment you type `p'...'`, you're dropped directly into the middle of your Perl main subroutine, with `$_` already set implicitly to the incoming line of the input stream.


####`r()`: Emit row

Up to this point we have not discussed how or what the Perl operator returns; it turns out that this is less intuitive than one might expect.

Let's take a look at the ouput of our script when we take out the `r` from inside the Perl mapper.

```
ni /usr/share/dict/words rx40 r10 p'substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)'
aba
iss
ed
aba
sta
rdize
abb
rev
iature
abd
uct
...
...
...
```

Without `r()`, every value separated by a comma is **returned** on its own row; these returned rows are then sent to the output stream.

The `r()` operator, on the other hand, **returns the empty list**. It works by printing the values separated by columns to a single tab-delimited row of the output stream.

Now that the practical differences between `r()` and `p'...'` have been explained, we can examine the differences in their use that are entailed.  

Clearly, If the desired output of the Perl mapper is two or more columns per row of stream data, you must use `r()`. If the desired output of the perl mapper step is a single column per row, you could either use `r()` or not.  The more concise statement leaving out `r()` is preferred.

When it is clear from context (as above), `r()'` can be referred to as  `r`, which is how it is more commonly written in practice. This differs from the take-rows operator (also called `r`).

####Column Accessor Functions `a` and `a()`

`ni` provides access to all of standard Perl 5, plus a number of functions that significantly increase the keystroke-efficiency and readability of `ni` spells.

The most fundamental of these are the column accessor functions `a(), b(), c(),..., l()`. These functions give access to the values of the first 12 columns of the input data. If you're wondering, the reason that there is no `m` is because it is a reserved character by the Perl system for writing regular expressions with nonstandard delimiters (e.g. pipes).

The functions `a() ... l()` are usually shortened to `a, b, c, ..., l` when their meanings would be unambiguous.  In general this is the case;  the one important exception to this rule is hash lookup, which requires that the user call the function explicitly.
 
Note that these functions do not pollute your namespace, so you can write confusing and pointless `ni` spells like this:

`ni n1 p'my $a = 5; r a, $a'`

If you can understand that, you're well on your way on mastering enough Perl to be proficient in `ni`.

Taking that a step farther, you can overwrite these functions if you want to rough `ni` up a bit. `ni` is pretty resilient; if you're feeling anarchic, you can overwrite these builtin functions.

`ni n1 p'sub a { "YO" }; my $a=19; r $a, a, $a, a' p'sub r { "HI" }; r, a, b, c, d' p'r substr(a, 0, 1)'`

```
Prototype mismatch: sub ni::pl::a () vs none at - line 411.
Prototype mismatch: sub ni::pl::r (@) vs none at - line 411.
H
1
Y
1
Y
(END)
```

Observe that rewriting `a()` in the first perl mapper had no effect on the functioning of `a` anywhere else; the same with rewriting `r()` in the second perl mapper.

This brings us to an important point about `ni` processes in general:

> `ni op1 op2` is equivalent to `ni op1 | ni op2`.

####`rp'...'`: Take rows based on Perl

We can combine the take-rows operator `r` with the Perl operator `p'...'` to create powerful filters. In this case, `r` will take all rows where the output of the Perl statement **is _truthy_ in Perl**.


Other caveats: the number 0, the string 0 (which is the same as the number 0), the empty list, the empty string, and the keyword `undef` are all **falsey** (i.e. interpreted as boolean false) in Perl. Pretty much everything else is truthy. There is no boolean True or False in Perl, so `false` and `False` are still truthy.

Examples:

* `ni n03 rp'$_'` -- rejects the first row, 0, which is falsey. It returns 1, 2 because the return value of the first row, 0, is falsey
* `ni n03 rp'a'` -- returns 1, 2 because the return value of the first row, 0, is falsey
* `ni n03 rp'r a'` -- rejects every row, but has an output equal to the initial stream (0, 1, 2). How does this happen?
  * The return value of `p'r a'` is the empty list, which is falsey; therefore, every row is rejected.
  * However, `r a` prints `a` to the stream as a side effect (regardless of the preceding row operator `r`). Thus, the whole stream is reconstituted.
* `ni n03 rp'r b'` -- prints 3 blank rows to the stream; the return value of `r()` is the empty list, so every row is rejected . `r()` side-effectually prints `b` for each row.


##Basic Column Operations
`$ ni /usr/share/dict/words rx40 r10 p'r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)' fCBrA`

```
ed      iss
rdize   sta
iature  rev
ion     uct
nt      tme
l       gai
e       udg
h       lis
vely    rti
(END)
```

Looking at the output, we see that it has 9 rows, rather than 10, and that those rows are composed of the third column and the second column of the data from the previous example.  The row that has disappeared is one that had nothing in the third column.

```
$ ni --explain  /usr/share/dict/words rx40 r10 p'r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)' fCBrA
["cat","/usr/share/dict/words"]
["row_every",40]
["head","-n",10]
["perl_mapper","r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)"]
["cols",3,2,1]
["row_cols_defined",1,0]
```

####`f`: Column Selection
Columns are indexed using letters, as in Excel. The `f` operator thus gives you access to the first 26 columns of your data. If your data has more than 26 columns, these fields can be accessed using the Perl field syntax, discussed later.

`r` followed by a column name will filter out all columns that have an empty value for that column.  Note that `r` is contextual here; once we have rearranged the data with `fCB` so that what was the third column is now in the first position (i.e. column `A`), we interact with it under its new alias.  Adding whitespace to the command `fCBrA` to become `fCB rA` is acceptable `ni` style, as we have added clarity with only a small decease in concisensess. 

Like `r`, there is a lot you can do with `f`:

* `$ ni <data> fAA` - select the first column and duplicate it
* `$ ni <data> fAD.` - select the first column and all remaining columns starting with the fourth
* `$ ni <data> fB-E` - select columns 2-5
* `$ ni <data> fCAHDF` - selects columns 3, 1, 8, 4, 6, and streams them out in that order.


####`x`: Column Exchange

`x` is a shorthand for certain operations that would otherwise be written using `f` with a greater number of keystrokes.

* `x` -- exchange the first two columns. 
  * Equivalent to `fBA.`
* `xC` -- exchange column 3 with column 1. 
  * Equivalent to `fCBA.`
* `xBE` -- exchange columns 2 and 5 with columns 1 and 2. 
  * This runs in order, so `B` will be switched with `A` first, which will then be switched with column `E`. 
  * Equivalent to `fBECDA.`

The spell for this exercise could equivalently be written:

`$ ni /usr/share/dict/words rx40 r10 p'r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)' fB.xrA`

##Column Operation Shortand
The operations in this section complete the set of column generation and access; none of them are particularly difficult to impelment in Perl, but they are common enough that `ni` has added shorthand for them.  The first set of operators, `F`, is used to create columns of data out of a stream of text columns. The second set `p'F_ ...', p'FM', and p'FR n'`, are used for general purpose access to columns in the stream.

####`F`: Split text stream into columns

* `F:<char>`: split on character
  * Note: some characters need to be escaped (for example, forward-slash); use `F/regex/` below for more flexibility (at the cost of less concision).
* `F/regex/`: split on occurrences of regex. If present, the first capture group will be included before a tab is appended to a field.
* `Fm/regex/`: don't split; instead, look for matches of regex and use those as the field values.
* `FC`: split on commas (doesn't handle special CSV cases)
* `FV`: parse CSV "correctly," up to newlines in fields
* `FS`: split on runs of horizontal whitespace
* `FW`: split on runs of non-word characters
* `FP`: split on pipe symbols

####`p'F_ ...'; p'FM'; p'FR n'`: Explicit field access

In general, `ni` data will be long and narrow--that is, it will have millions to trillions of rows in the stream, but usually no more than a dozen relevant features per row.

However, `ni` implements access to fields beyond the first 12 using the explicit field accessors `p'F_ ...', p'FM', p'FR n'`

It has not occurred in my experience that I have needed to maintain more than 12 relevant columns, and as a result I find the syntax a bit hard to remember, because I think of `A` as the first letter, rather than the zeroth, which is how `ni` thinks, internally.

`p'F_ ...'` takes a range (or a single number) as an optional second argument. `p'F_` by itself returns all fields of the input stream.

To print fields 11-15 of a data source, you would use `$ ni ... r F_ 10..14`.

The index of the final nonempty field in a line is stored in `FM`. To get the last field of every line in our example, you could use the spell: `$ ni /usr/share/dict/words rx40 r10 p'r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)' p'r FM, F_ FM'`

It is often useful to take everything after a certain point in a line. This can be accomplished efficiently using the `FR n` operator. `FR n` is equivalent to `F_ n..FM`.



##Sort, Unique, and Count
Sorting large amounts of data requires buffering to disk, and the vagaries of how this works internally is a source of headaches and slow performance. If your data is larger than a gigabyte uncompressed, you may want to take advantage of massively distributing the workload through Hadoop operations.

If you're not convinced that anything could go slow in `ni`, let's try counting all of the letters in the dictionary 

`$ ni /usr/share/dict/words F// pF_ gc \>letter_counts.txt`

You'll probably see the `ni` monitor for the first time, showing the steps of the computation, and what steps are taking time.  The whole process takes about 90 seconds on my computer.

```
$ ni --explain /usr/share/dict/words F// pF_ gc \>letter_counts.txt
ni --explain /usr/share/dict/words F// pF_ gc \>letter_counts.txt
["cat","/usr/share/dict/words"]
["split_regex",""]
["perl_mapper","F_"]
["row_sort","-t","\t"]
["count"]
["file_write","letter_counts.txt"]
```

What's interesting here is that your Unix dictionary is probably only about 2.5 MB in size; the dictionary itself can be streamed into memory in a fraction of a second. Remembering that all `ni` development code such that it is I/O bounded; in this case, there is an I/O bounded step where the data must be written to disk in order to be sorted. One can avoid this bound by adding `r10` after the filename, however.


#### `g`: General sorting
`g` is the most general sorting operator; there are two other sorting operators within `ni`, but they are highly specific.

With a single column of data, as in the example, the simple command `g` will give you lexicographic sorting **in ASCII, in ascending order**. 

**Feature test: Unicode**

To do more complicated sorts, you can give `g` columns and modifiers. As with `f`, columns are indexed `A-Z`. `g` has two modifiers, `n` and `-`. `n` makes the sort numeric, and `-` makes the sort descending, rather tahn ascending.

Examples: 

`$ ni /usr/share/dict/words F// p'FR 0' gc =\>letter_counts.txt gAn =\>ascending_letter_counts.txt gB- \>counts_by_letter_reversed.txt`

As above, if you have more than one column, you currently **must** specify the columns you want sorted; the reason for this is a system-to-system instability with regard to how the unix `sort` interacts).
  
####`c`: Count Sorted Rows
`c` is `ni`'s version of `uniq -c`, which counts the number of identical  consecutive rows in a stream. The main difference is that `ni`'s `c` tab-delimits the output, while `uniq -c` space-delimits.
   
####`u`: Unique Sorted Rows
`u` is `ni` syntax for `uniq`, which takes sorted rows and returns the unique values.
  
####`o` and `O`: Syntactic Sugar for Numeric Sorting
Often you will want numeric sorting in a more keystroke-efficient way than `gn<column>-`. The `o` (sort rows ascending, numerically) and `O` (sort rows  descending, numerically) operator has been provided for this purpose.

The command from the `g` section can be rewritten as:

`$ ni /usr/share/dict/words F// p'FR 0' gc =\>letter_counts.txt oA =\>ascending_letter_counts.txt gB- \>counts_by_letter_reversed.txt`

**Important Note**: `o` and `O` sorts *cannot be chained together* or combined with `g`. If you write a command like `$ ni ... gAoB`, there is no guarantee that it  will have a lexicographically sorted first column. If you want to sort by the first column ascending lexicographically and the second column ascending numerically in the same sort, you should use a more explicit `g` operator: `$ni ... gABn`.

##Conclusion of Chapter 1
Congrats on making it to the end of the first part. Hopefully you're starting to see the power in `ni`'s conciseness. If you haven't gotten a chance to develop or play with `ni` code yet, there will likely be some accompanying exercises for this tutorial in the near future, or you can write some yourself and contribute to the development of this fascinating language.

If you've only done this tutorial, you might be a little disappointed that your productivity as a programmer hasn't increased by much yet. Don't worry; when we start talking about Hadoop streaming operations in [Chapter 2](ni_by_example_2.md), you'll see your productivity grow by leaps and bounds. We'll see you in the next tutorial.
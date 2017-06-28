# `ni` by Example, Chapter 1 (alpha release)

Welcome! This is a "rich" tutorial that covers all of the basics of this cantankerous, odd, and ultimately, incredibly fast, joyful, and productive tool called `ni`. We have tried to assume as little knowledge as possible in this tutorial, but if you find anything confusing, please contact [the developers](http://github.com/spencertipping) or [the author](http://github.com/michaelbilow).

`ni` suffers from having a steep learning curve, and while the knowledge represented in this tutorial is fundamental, it only hints at `ni`'s real power, which is its virality. This tutorial is structured in 5 parts:

1. Intro to `ni`
2. Perl for `ni`
3. The real power of `ni`
4. `ni` and Ruby, Perl, Python, Lisp & Bash
5. `ni` odds and ends
6. `ni` + Jupyter (TODO)


## What is `ni`?

`ni` is write-anywhere, run-everywhere

`ni` is a self-modifying quine.

`ni` is everything-from-the-command-line.

`ni` is concise.

`ni` is beautiful.

`ni` is two-fisted data science.


## Installation
`ni` works on any Unix-based OS. You should use a bash prompt when calling `ni`.

```
git clone git@github.com:spencertipping/ni.git
cd ni
ln -s $PWD/ni ~/bin/ni  # or whatever to add it to your path
```


## Basic Stream Operators

`ni` is a stream-processing language. Most operations in `ni` are done over a single line, which enables `ni` to be fast and memory-efficient. 

`ni` commands are composed of operators, examples include `n`, which generates a stream of integers; `g`, which sorts the stream; `z`, which compresses a stream, and `HS`, which executes a Hadoop Streaming MapReduce job. 

### `n`: Integer Stream
`ni n` generates a stream of consecutive integers starting at 1. The number after determines how many numbers will be generated.

```bash
$ ni n5
1
2
3
4
5
```

In general, `ni` will drop you into a `less` pager after a command finishes. You can change the default pager by setting the `NI_PAGER` environment variable.

`ni n0` gives you consecutive integers starting from zero. For example:

```bash
$ ni n03
0
1
2
```

To generate a large  number of integers, you can use scientific notation with `n`. `ni n3.2E5` will give you 320,000 consecutive integers, starting from 1.


### `i`: Literal text 
The `i` operator puts literal text into the stream:

```bash
$ ni ihello
hello
```

You can use quotes with `i` to include spaces within strings.

```bash
$ ni i"hello there"
hello there
```

`ni` is optimized to work with tab-delimited text. If you want your text to be tab-delimited, put your text inside square brackets.

```bash
$ ni i[hello there new friend]
hello	there	new	friend
```

### `e'...'`: Evaluate `bash` script

`ni` is deeply connected to bash, so easy access is provided to running bash commands from within `ni`.  

```bash
$ ni n500 e'grep 22'
22
122
220
221
222
223
224
225
226
227
228
229
322
422
```


### Structure of `ni` commands

In the last section, you saw a `ni` command linking two operators; `n500` was used to generate a stream of integers from 1 to 500, and the `e'grep 22'` was used to take the lines that had a `22` in them. If you're not used to working with streams, there's a slightly subtle point to notice.

In general commands are written `ni <op_1> <op_2> ... <op_n>`. It is often helpful to think of each command by piping the output of one command to the input of the next `ni <op_1> | ni <op_2> | ... | ni <op_n>`.

The more compressed syntax is favored, and very short commands are often compressed without spaces in between. A common example is sort (`g`) + unique (`u`); this is commonly written as `gu` in rather than the more explicit `g u`. Because `ni` commands are highly compressed and difficult to be read by people who are uninitiated, they are sometimes referred to as "spells."

## Streaming I/O

`ni` provides very flexible file input and output. In particular, compressing streams in `ni` is very simple, and decompression is done by default.


### `>`: `bash`-style file output

`ni` is a streaming tool, so you can redirect output using the standard redirect.

```
$ ni n10 > ten.txt
```

While this works fine, it is in general not used, in favor of the literal angle-bracket operator `\>` described in the next section.

### `\>`: Output and Emit File Name

`ni ... \>ten.txt` outputs the stream to a file called `ten.txt` and emits the file name in a `less` pager.

```bash
$ ni n10 \>ten.txt
ten.txt
```

Note that there is **no space** between `\>` and `ten.txt`. Because `ni` is concise, whitespace is frequently important.

### `\<` Read from Filenames

The reason that `\>` is favored over `>` is because `\<` inverts it by reading the data from filenames.

```bash
$ ni n10 \>ten.txt \<
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
```


### File Input

When running on multiple files (or the same file), the output is appended to the stream. For example:

```
$ ni ten.txt ten.txt
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
```

### `z`: Compression

`ni` provides compression in a highly keystroke-efficient way;

```
$ ni n10 z \>ten.gz
ten.gz
```


The default compression with `z` is `gzip`, however there are options for bzip, xzip, lzo, and lz4.

`ni` decompresses its input by default. We can take our output and look at it in `ni` very easily using `$ ni ten.gz` or `$ cat ten.gz | ni`.


```bash
$ ni n10 z \>ten.gz \<
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
```


## `ni` Coding and Debugging

The simplest way to build up a `ni` spell is by writing one step of the spell, checking that step's output for correctness, then writing another step.

In general, `ni` spells will start producing output very quickly (or can be coerced to produce output quickly). Once the output of one step in the spell looks good, you can move on to the next step.

As you advance through this tutorial, you'll want a quicker way to understand at a high level what a particular `ni` spell is doing.


### `--explain`: Print information on command

For this, use `ni --explain ...`. Using the example from the file output section:

```bash
$ ni --explain n10 \>ten.txt \<
["n",1,11]
["file_write","ten.txt"]
["file_read"]
```

Each line represents one step of the pipeline defined by the spell, and the explanation shows how the `ni` parser interprets what's written. Often these are rich-text explanations 



## Basic Row Operations, File Reading, and Output Redirection


### `r`: Take Rows

#### Numeric options for `r`
`r` is a powerful and flexible operation for filtering rows. Here's a short demonstration of its abilities. Eventually you will have all of these operations memorized, but for now, just try to remember that you have these options available to you.

```bash
$ ni n10 r3
1
2
3
```

```bash
$ ni n10 r-3
4
5
6
7
8
9
10
```

```bash
$ ni n10 r~3
8
9
10
```

```bash
$ ni n10 rx3
3
6
9
```


Adding a number between 0 and 1 will lead ot 
```
$ ni n10 r.15
```

#### `r`: regex options

`r` also can take a regex: `$ ni <data> r/<regex>/` takes all rows where the regex has a match.

For example, you can take all of the numbers that end in 0, 1, or 5 with the following spell: `$ ni n20 r/[015]$/`

Because you're typing directly into the `bash` shell, some characters may need to be escaped, for example in this expression which identifies numbers that are all repeating digits.

`$ ni n1000 r/^\(\\d\)\\1+$/`

Use of escape characters in `ni` operators is acceptable style only if there is not a conciser way that is at least as readable, or a more readable way that is at least as concise. way to do the job. In this case, there is both. We can quote the regex inside of single quotes to make `r` filter on rows that match the `ni n1000 r'/^(\d)\1+$/'`

The `r` operator is especially useful during development; for example, if you are working with a large file or stream, you can check the correctness of your output using `r10`, `rx100`, `r.001` etc. to downsample and cap the amount of data read.  

> With the exception of operators that require processing the entire stream (sorting, for example) all `ni` development can be I/O-bounded, and not processor-bounded, regardless of the resources required by the computation.



## Perl Operations
`ni` and Perl go well together philosophically. Both have deeply flawed lexicons and both are highly efficient in terms of developer time and processing time. `ni` and Perl scripts are both difficult to read to the uninitiated. They demand a much higher baseline level of expertise to understand what's going on than, for example, a Python script. 

In addition to Perl, `ni` offers direct interfaces to Ruby and Lisp. While all three of the languages are useful and actively maintained, `ni` is written in Perl, and it is by far the most useful of the three. If you haven't learned Perl yet, but you're familiar with another scripting language, like Python or Ruby, I found [this course](https://www.udemy.com/perltutorial/learn/v4/) on Udemy useful for learning Perl's odd syntax.

We'll start with the following `ni` spell.

`$ ni /usr/share/dict/words rx40 r10 p'r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)'`

The output here will depend on the contents of your `/usr/share/dict/words/`, but you should have 3 columns; the first 3 letters of each word, the second 3 letters of each word, and any remaining letters. On my machine it looks like this:

```sh
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
```

Notice that `ni` has produced tab-delimited columns for us; these will be useful for the powerful column operators we will introduce in this section and the next.

```bash
$ ni --explain /usr/share/dict/words rx40 r10 p'r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)'
["cat","/usr/share/dict/words"]
["row_every",40]
["head","-n",10]
["perl_mapper","r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)"]
```

We have reviewed every operator previously except the last. 


### `p'...'`: Map Perl over lines
When you think of writing a simple data processing program in Python, Ruby, C, or even Perl, think about how many keystrokes are spent loading libraries that are used mostly implicitly; and if they're not loaded, the program won't run.

Even the act of writing a script that reads from standard input and writes to standard output, maybe compiling it, and then calling it with arguments from the command line requires a lot of task-switching.  

`ni` removes all of that; the moment you type `p'...'`, you're dropped directly into the middle of your Perl main subroutine, with `$_` already set implicitly to the incoming line of the input stream.


### `r()`: Emit row

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

### `1`: Dummy pulse
Suppose that you have a Perl script that generates data, and you want to generate data from that script directly. You might be surprised that the following code generates no output:

```
$ ni p'for(my $i = 1; $i <= 5; $i++) {r map $i * $_, 1..3}'
```

One of the complicated aspects of `ni` is that the Perl operator `p'...'` requires an input stream to run. In this case, the number of lines in the input stream will determine the number of times the Perl mapper is run.

In order to cause a script to execute, `ni` provides the `1` operator, which provides a pulse to run the stream. `1` is syntactic sugar for `n1`, which would work just as well here.

```bash
$ ni 1p'for my $i (1..5) {r map $i * $_, 1..3}'
1	2	3
2	4	6
3	6	9
4	8	12
5	10	15
```

Several other operators also require a pulse to run, including the Numpy, Ruby, and Lisp operators, which will be covered in more detail in later chapters.

### Column Accessor Functions `a` and `a()`

`ni` provides access to all of standard Perl 5, plus a number of functions that significantly increase the keystroke-efficiency and readability of `ni` spells.

The most fundamental of these are the column accessor functions `a(), b(), c(),..., l()`. These functions give access to the values of the first 12 columns of the input data. If you're wondering, the reason that there is no `m` is because it is a reserved character by the Perl system for writing regular expressions with nonstandard delimiters (e.g. pipes).

The functions `a() ... l()` are usually shortened to `a, b, c, ..., l` when their meanings would be unambiguous.  In general this is the case;  the one important exception to this rule is hash lookup, which requires that the user call the function explicitly.
 
Note that these functions do not pollute your namespace, so you can write confusing and pointless `ni` spells like this:

`ni 1p'my $a = 5; r a, $a'`

If you can understand that, you're well on your way on mastering enough Perl to be proficient in `ni`.

Taking that a step farther, you can overwrite these functions if you want to rough `ni` up a bit. `ni` is pretty resilient; if you're feeling anarchic, you can overwrite these builtin functions.

`ni 1p'sub a { "YO" }; my $a=19; r $a, a, $a, a' p'sub r { "HI" }; r, a, b, c, d' p'r substr(a, 0, 1)'`

```
Prototype mismatch: sub ni::pl::a () vs none at - line 411.
Prototype mismatch: sub ni::pl::r (@) vs none at - line 411.
H
1
Y
1
Y
```

Observe that rewriting `a()` in the first perl mapper had no effect on the functioning of `a` anywhere else; the same with rewriting `r()` in the second perl mapper.

This brings us to an important point about `ni` processes in general:

> `ni op1 op2` is equivalent to `ni op1 | ni op2`.

### `rp'...'`: Take rows based on Perl

We can combine the take-rows operator `r` with the Perl operator `p'...'` to create powerful filters. In this case, `r` will take all rows where the output of the Perl statement **is _truthy_ in Perl**.


Other caveats: the number 0, the string 0 (which is the same as the number 0), the empty list, the empty string, and the keyword `undef` are all **falsey** (i.e. interpreted as boolean false) in Perl. Pretty much everything else is truthy. There is no boolean True or False in Perl, so `false` and `False` are still truthy.

Examples:

* `ni n03 rp'$_'` -- rejects the first row, 0, which is falsey. It returns 1, 2 because the return value of the first row, 0, is falsey
* `ni n03 rp'a'` -- returns 1, 2 because the return value of the first row, 0, is falsey
* `ni n03 rp'r a'` -- rejects every row, but has an output equal to the initial stream (0, 1, 2). How does this happen?
  * The return value of `p'r a'` is the empty list, which is falsey; therefore, every row is rejected.
  * However, `r a` prints `a` to the stream as a side effect (regardless of the preceding row operator `r`). Thus, the whole stream is reconstituted.
* `ni n03 rp'r b'` -- prints 3 blank rows to the stream; the return value of `r()` is the empty list, so every row is rejected . `r()` side-effectually prints `b` for each row.


## Basic Column Operations


```$ ni /usr/share/dict/words rx40 r10 p'r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)' fCBrA
ed      iss
rdize   sta
iature  rev
ion     uct
nt      tme
l       gai
e       udg
h       lis
vely    rti
```

Looking at the output, we see that it has 9 rows, rather than 10, and that those rows are composed of the third column and the second column of the data from the previous example.  The row that has disappeared is one that had nothing in the third column.

```bash
$ ni --explain  /usr/share/dict/words rx40 r10 p'r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)' fCBrA
["cat","/usr/share/dict/words"]
["row_every",40]
["head","-n",10]
["perl_mapper","r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)"]
["cols",3,2,1]
["row_cols_defined",1,0]
```

### `f`: Column Selection
Columns are indexed using letters, as in Excel. The `f` operator thus gives you access to the first 26 columns of your data. If your data has more than 26 columns, these fields can be accessed using the Perl field syntax, discussed later.

`r` followed by a column name will filter out all columns that have an empty value for that column.  Note that `r` is contextual here; once we have rearranged the data with `fCB` so that what was the third column is now in the first position (i.e. column `A`), we interact with it under its new alias.  Adding whitespace to the command `fCBrA` to become `fCB rA` is acceptable `ni` style, as we have added clarity with only a small decease in concisensess. 

Like `r`, there is a lot you can do with `f`:

* `$ ni <data> fAA` - select the first column and duplicate it
* `$ ni <data> fAD.` - select the first column and all remaining columns starting with the fourth
* `$ ni <data> fB-E` - select columns 2-5
* `$ ni <data> fCAHDF` - selects columns 3, 1, 8, 4, 6, and streams them out in that order.


### `x`: Column Exchange

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

## Column Operation Shortand
The operations in this section complete the set of column generation and access; none of them are particularly difficult to impelment in Perl, but they are common enough that `ni` has added shorthand for them.  The first set of operators, `F`, is used to create columns of data out of a stream of text columns. The second set `p'F_ ...', p'FM', and p'FR n'`, are used for general purpose access to columns in the stream.

### `F`: Split text stream into columns

* `F:<char>`: split on character
  * Note: some characters need to be escaped (for example, forward-slash); use `F/regex/` below for more flexibility (at the cost of less concision).
* `F/regex/`: split on occurrences of regex. If present, the first capture group will be included before a tab is appended to a field.
* `Fm/regex/`: don't split; instead, look for matches of regex and use those as the field values.
* `FC`: split on commas (doesn't handle special CSV cases)
* `FD`: split on forward slashes
* `FV`: parse CSV "correctly," up to newlines in fields
* `FS`: split on runs of horizontal whitespace
* `FW`: split on runs of non-word characters
* `FP`: split on pipe symbols

### `p'F_ ...'; p'FM'; p'FR n'`: Explicit field access

In general, `ni` data will be long and narrow--that is, it will have millions to trillions of rows in the stream, but usually no more than a dozen relevant features per row.

However, `ni` implements access to fields beyond the first 12 using the explicit field accessors `p'F_ ...', p'FM', p'FR n'`

It has not occurred in my experience that I have needed to maintain more than 12 relevant columns, and as a result I find the syntax a bit hard to remember, because I think of `A` as the first letter, rather than the zeroth, which is how `ni` thinks, internally.

`p'F_ ...'` takes a range (or a single number) as an optional second argument. `p'F_` by itself returns all fields of the input stream.

To print fields 11-15 of a data source, you would use `$ ni ... r F_ 10..14`.

The index of the final nonempty field in a line is stored in `FM`. To get the last field of every line in our example, you could use the spell: `$ ni /usr/share/dict/words rx40 r10 p'r substr(a, 0, 3), substr(a, 3, 3), substr(a, 6)' p'r FM, F_ FM'`

It is often useful to take everything after a certain point in a line. This can be accomplished efficiently using the `FR n` operator. `FR n` is equivalent to `F_ n..FM`.



## Sort, Unique, and Count
Sorting large amounts of data requires buffering to disk, and the vagaries of how this works internally is a source of headaches and slow performance. If your data is larger than a gigabyte uncompressed, you may want to take advantage of massively distributing the workload through Hadoop operations.

If you're not convinced that anything could go slow in `ni`, let's try counting all of the letters in the dictionary 

`$ ni /usr/share/dict/words F// pF_ gc \>letter_counts.txt`

You'll probably see the `ni` monitor for the first time, showing the steps of the computation, and what steps are taking time.  The whole process takes about 90 seconds on my computer.

```bash
$ ni --explain /usr/share/dict/words F// pF_ gc \>letter_counts.txt
["cat","/usr/share/dict/words"]
["split_regex",""]
["perl_mapper","F_"]
["row_sort","-t","\t"]
["count"]
["file_write","letter_counts.txt"]
```

What's interesting here is that your Unix dictionary is probably only about 2.5 MB in size; the dictionary itself can be streamed into memory in a fraction of a second. Remembering that all `ni` development code such that it is I/O bounded; in this case, there is an I/O bounded step where the data must be written to disk in order to be sorted. One can avoid this bound by adding `r10` after the filename, however.


### `g`: General sorting
`g` is the most general sorting operator; there are two other sorting operators within `ni`, but they are highly specific.

With a single column of data, as in the example, the simple command `g` will give you lexicographic sorting **in ASCII, in ascending order**. 

**Feature test: Unicode**

To do more complicated sorts, you can give `g` columns and modifiers. As with `f`, columns are indexed `A-Z`. `g` has two modifiers, `n` and `-`. `n` makes the sort numeric, and `-` makes the sort descending, rather tahn ascending.

Examples: 

`$ ni /usr/share/dict/words F// p'FR 0' gc =\>letter_counts.txt gAn =\>ascending_letter_counts.txt gB- \>counts_by_letter_reversed.txt`

As above, if you have more than one column, you currently **must** specify the columns you want sorted; the reason for this is a system-to-system instability with regard to how the unix `sort` interacts).
  
### `c`: Count Sorted Rows
`c` is `ni`'s version of `uniq -c`, which counts the number of identical  consecutive rows in a stream. The main difference is that `ni`'s `c` tab-delimits the output, while `uniq -c` space-delimits.
   
### `u`: Unique Sorted Rows
`u` is `ni` syntax for `uniq`, which takes sorted rows and returns the unique values.
  
### `o` and `O`: Syntactic Sugar for Numeric Sorting
Often you will want numeric sorting in a more keystroke-efficient way than `gn<column>-`. The `o` (sort rows ascending, numerically) and `O` (sort rows  descending, numerically) operator has been provided for this purpose.

The command from the `g` section can be rewritten as:

`$ ni /usr/share/dict/words F// p'FR 0' gc =\>letter_counts.txt oA =\>ascending_letter_counts.txt gB- \>counts_by_letter_reversed.txt`

**Important Note**: `o` and `O` sorts *cannot be chained together* or combined with `g`. If you write a command like `$ ni ... gAoB`, there is no guarantee that it  will have a lexicographically sorted first column. If you want to sort by the first column ascending lexicographically and the second column ascending numerically in the same sort, you should use a more explicit `g` operator: `$ni ... gABn`.

## Conclusion
Congrats on making it to the end of the first part. Hopefully you're starting to see the power in `ni`'s conciseness. If you haven't gotten a chance to develop or play with `ni` code yet, there will likely be some accompanying exercises for this tutorial in the near future, or you can write some yourself and contribute to the development of this fascinating language.

If you've only done this tutorial, you might be a little disappointed that your productivity as a programmer hasn't increased by much yet. Don't worry; when we start talking about Hadoop streaming operations in [Chapter 2](ni_by_example_2.md), you'll see your productivity grow by leaps and bounds. We'll see you in the next tutorial.
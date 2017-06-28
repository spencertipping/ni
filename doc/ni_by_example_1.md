# `ni` by Example, Chapter 1 (beta release)

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

```sh
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

`ni` provides compression in a highly keystroke-efficient way, using the `z` operator.

```bash
$ ni n10 z \>ten.gz
ten.gz
```



The default compression with `z` is `gzip`, however there are options for bzip (`zb`), xzip (`zx`), lzo (`zo`), and lz4 (`z4`). You can also specify a numeric flag to gzip by using a number other than `4`, for example `z9` executes `gzip -9` on the stream.

`ni` decompresses its input by default. 

```sh
cat ten.gz
2�SY3�2�2�2�2�2�2���24�뿊
```

```sh
$ cat ten.gz | ni
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




## Basic Row and Column Operations

`ni` works especially well on data formatted as tab-delimited rows of text; in this section we'll show how to filter, trim, and convert raw text into tab-delimited form.

### `r`: Take rows

`r` is a powerful and flexible operation for filtering rows; it encompasses the functionality of Unix operators `head` and `tail`, as well as a large number of filtering operations.

#### Numeric Options

`rN` takes the first N rows.

```bash
$ ni n10 r3
1
2
3
```

`r-N` drops the first N rows.
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

`r~N` takes the last N rows.

```bash
$ ni n10 r~3
8
9
10
```

The `rxN` option takes the last of every `N` rows.

```bash
$ ni n10 rx3
3
6
9
```

Adding a number between 0 and 1 will lead to `ni` selecting a (deterministic) pseudo-random sample of the stream data.

```bash
$ ni n20 r.15
1
9
11
12
14
15
```

These last examples show the value of `r` in development; for example, if you are working with a large file or stream, you can check the correctness of your output using `r10`, `rx100`, `r.001` etc. to view smaller samples of large datasets.

#### Logical Options

`r` can also be thought of as "take rows where the predicate that follows evaluates to true." In this context, `r` can take a regex as an option: `$ ni <data> r/<regex>/` takes all rows where the regex has a match. We can rewrite our example for `e` `$ ni n500 e'grep 22'` as:

```bash
$ ni n500 r/22/
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

To use escaped characters in a regex, it's often helpful to wrap in quotes:

```bash
$ ni n1000 r-500 r'/^(\d)\1+$/'
555
666
777
888
999
```

In `ni`, tab-delimited columns of data are referenced like a spreadsheet: the first column is `A`, the second is `B`, etc.

We've seen how to generate tab-delimited columns using the `i` operator with brackets:

```bash
$ ni i[one_column] i[two columns] i[three columns here]
one_column
two     columns
three   columns here
```

Because an empty column is falsey (it evaluates to false), we can filter it using `r`. 

```bash
$ ni i[one_column] i[two columns] i[three columns here] rB
two     columns
three   columns here
```

### `F`: Split stream into columns

Like `r`, `F` also has many options. The most important is `F/regex/`, which splits text into columns based on a regular expression.

```bash
$ ni ibubbles ibaubles ibarbaras F/[aeiou]+/
b       bbl     s
b       bl      s
b       rb      r       s
```

The rest of the operations are syntactic sugar, but they're worth knowing.


`FD` splits on forward slashes:

```bash
$ ni i~/bin/dependency/nightmare.jar FD
~	bin	dependency	nightmare.jar
```

`FS` splits on runs of whitespace:

```bash
$ ni i"here               is   an              example" FS
here	is	an	example
```

`FC` splits on commas, but doesn't handle CSV:

```bash
$ ni ibread,eggs,milk i'fruit gushers,index cards' FC
bread	eggs	milk
fruit gushers	index cards
```

`FV` splits CSV fields correctly (i.e. it doesn't split commas in quoted strings). However, `ni` splits lines of input on newline characters, so this can't handle newlines in quoted fields.

```bash
$ ni i"hello,there",one,two,three
hello,there	one	two	three
```

`FW` splits on non-word characters (i.e. equivalent to splitting on the regex  `/\W+/`)

```bash
ni i'this@#$$gets&(*&^split' FW
this	gets	split
```

`FP` splits on pipe characters:
```bash
$ ni i'need|quotes|around|pipes|because|of|bash' FP
need	quotes	around	pipes	because	of	bash
```

`F:<char>` splits data on a particular character:

```bash
$ ni ibubbles ibaubles ibarbaras F:a
bubbles
b	ubles
b	rb	r	s
```


### `f`: Column Selection
Columns are indexed using letters, as in Excel. The `f` operator gives you access to the first 26 columns of your data. If your data has more than 26 columns, these fields can be accessed using the Perl field syntax, discussed later.

Let's start by generating some text and converting it to columns using `F`.

```bash
$ ni i"this is how we do it" i"it's friday night" i"and I feel all right" FS
this	is	how	we	do	it
it's	friday	night
and	I	feel	all	right
```

Like `r`, `f` has a lot of options. To select a column, use its corresponding letter. 

```bash
$ ni i"this is how we do it" i"it's friday night" i"and I feel all right" FS fC
how
night
feel
```

You can select multiple columns by providing multiple letters:

```bash
$ ni i"this is how we do it" i"it's friday night" i"and I feel all right" FS fAB
this	is
it's	friday
and	I
```

You can duplicate a column by using its corresponding letter multiple times:

```bash
$ ni i"this is how we do it" i"it's friday night" i"and I feel all right" FS fAAC
this    this    how
it's    it's    night
and     and     feel
```

To select all columns after a particular column, use `f<col>.`
```bash
ni i"this is how we do it" i"it's friday night" i"and I feel all right" FS fAD.
this	we	do	it
it's
and	all	right
```

To select all data between two columns, inclusive, use a dash:
```bash
$ ni i"this is how we do it" i"it's friday night" i"and I feel all right" FS fB-E
is	how	we	do
friday	night
I	feel	all	right
```

You can also use `f` to re-order selected columns:

```bash
$ ni i"this is how we do it" i"it's friday night" i"and I feel all right" FS fCBAD
how	is	this	we
night	friday	it's
feel	I	and	all
```


Under the hood, `ni` is using either `cut` or Perl to rearrange the columns. `cut` is much faster of the two, but it's only used when the output columns are in the same relative order as the input columns.

**WARNING**: Once an `f` operation is completed, `ni` forgets about any previous ordering of the columns.



### `x`: Column Exchange

`x` is a shorthand for certain operations that would otherwise be written using `f` with a greater number of keystrokes.

* `x` -- exchange the first two columns. 
  * Equivalent to `fBA.`
* `xC` -- exchange column 3 with column 1. 
  * Equivalent to `fCBA.`
* `xBE` -- exchange columns 2 and 5 with columns 1 and 2. 
  * This runs in order, so `B` will be switched with `A` first, which will then be switched with column `E`. 
  * Equivalent to `fBECDA.`



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

### Staying in a command-line environment

`ni` is a bottom-up, ad hoc language; `ni` spells can be developed efficiently from the command line, or from a command line-like environment, like a Jupyter notebook.


## Conclusion
Congrats on making it to the end of the first part. Hopefully you're starting to see the power in `ni`'s conciseness. If you haven't gotten a chance to develop or play with `ni` code yet, there will likely be some accompanying exercises for this tutorial in the near future, or you can write some yourself and contribute to the development of this fascinating language.

If you've only done this tutorial, you might be a little disappointed that your productivity as a programmer hasn't increased by much yet. Don't worry; when we start talking about Hadoop streaming operations in [Chapter 2](ni_by_example_2.md), you'll see your productivity grow by leaps and bounds. We'll see you in the next tutorial.
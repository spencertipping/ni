# `ni` by Example, Chapter 1 (beta release)

Welcome! This is a "rich" tutorial that covers all of the basics of this cantankerous, odd, and ultimately, incredibly fast, joyful, and productive tool called `ni`. We have tried to assume as little knowledge as possible in this tutorial, but if you find anything confusing, please contact [the developers](http://github.com/spencertipping) or [the author](http://github.com/michaelbilow).

`ni` and Perl both suffer from their sharp differences from . This tutorial is structured in 6 parts:

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

`ni` commands are composed of operators. Examples introduced in the last section include `n`, which generates a stream of integers; `e`, which executes a bash command, and `i`, which puts literal text to the stream. `z`, which compresses a stream. Later, we'll introduce more complex operators like `HS`, whichexecutes a Hadoop Streaming MapReduce job. 

In the last section, you saw a `ni` command linking two operators; `n500` was used to generate a stream of integers from 1 to 500, and the `e'grep 22'` was used to take the lines that had a `22` in them. If you're not used to working with streams, there's a slightly subtle point to notice.

In general commands are written `ni <op_1> <op_2> ... <op_n>`. It is often helpful to think of each command by piping the output of one command to the input of the next `ni <op_1> | ni <op_2> | ... | ni <op_n>`.

More compressed syntax is favored, and very short commands are often compressed without spaces in between. A common example is sort (`g`) + unique (`u`); this is commonly written as `gu` in rather than the more explicit `g u`. Because `ni` commands are highly compressed and difficult to be read by people who are uninitiated, they are sometimes referred to as "spells."

## Streaming I/O

`ni` provides very flexible file input and output. In particular, compressing streams in `ni` is very simple, and decompression is done by default.


### `>`: `bash`-style file output

`ni` is a streaming tool, so you can redirect output using the standard redirect.

```sh
$ ni n5 > five.txt
```

While this works fine, it is in general not used, in favor of the literal angle-bracket operator `\>` described in the next section.


### File Input

To add a file to the stream in `ni`, add the name of the file to the stream.

```sh
$ ni five.txt
1
2
3
4
5
```


### `\>`: Output and Emit File Name


```bash
$ ni n5 \>five.txt
five.txt
```

Note that there is **no space** between `\>` and `ten.txt`. Because `ni` is concise, whitespace is frequently important.

### `\<` Read from Filenames

The reason that `\>` is favored over `>` is because `\<` inverts it by reading the data from filenames.

```bash
$ ni n5 \>five.txt \<
1
2
3
4
5
```

It's important to understand how this short spell works; first, a stream of 5 integers is generated; those integers sink to a file called `five.txt`, and the text string `five.txt` is put out to the stream. Finally, `\<` instructs `ni` to open the file named `five.txt` and put its contents on the stream.

### Directory I/O
The contents of this section 

Start by making some data:

```sh
$ rm -rf dir_test && mkdir dir_test
$ echo "hello" > dir_test/file1
$ echo "you" > dir_test/file2
$ echo "genius" > dir_test/file3
```

We can look at the contents of the directory with `ni`:

```sh
$ ni dir_test
dir_test/file1
dir_test/file2
```

`ni` has converted the folder into a stream of the names of files (and directories) inside it. You can thus access the files inside a directory using `\<`.

```sh
$ ni dir_test \<
hello
you
genius
```

`ni` will use bash expansion as well:

```sh
$ ni dir_test/*
hello
you
genius
```


```sh
$ ni dir_test/file{1,3}
hello
genius
```


### `z`: Compression

`ni` provides compression in a highly keystroke-efficient way, using the `z` operator.

```bash
$ ni n10 z \>ten.gz
ten.gz
```

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

The default compression with `z` is `gzip`, however there are options for bzip (`zb`), xzip (`zx`), lzo (`zo`), and lz4 (`z4`). You can also specify a numeric flag to gzip by using a number other than `4`, for example `z9` executes `gzip -9` on the stream.


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

`r~N` and `r+N` take the last N rows.

```bash
$ ni n10 r~3
8
9
10
```

```bash
$ ni n10 r+3
8
9
10
```

The `rxN` option takes the first of every `N` rows.

```bash
$ ni n10 rx3
1
4
7
10
```

Adding a number between 0 and 1 will lead to `ni` selecting a (deterministic) pseudo-random sample of the stream data.

```bash
$ ni n20 r.15
1
3
5
14
```

These last examples show the value of `r` in development; for example, if you are working with a large file or stream, you can check the correctness of your output using `r10`, `rx100`, `r.001` etc. to view smaller samples of large datasets.


### `F`: Split stream into columns

Like `r`, `F` also has many options. The most important is `F/regex/`, which splits text into columns based on a regular expression.

```bash
$ ni ibubbles ibaubles ibarbaras F/[aeiou]+/
b	bbl	s
b	bl	s
b	rb	r	s
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
$ ni i'"hello,there",one,two,three' FV
hello,there	one	two	three
```

`FW` splits on non-word characters (i.e. equivalent to splitting on the regex  `/\W+/`)

```bash
$ ni i'this@#$$gets&(*&^split' FW
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

Let's start by generating some text and converting it to columns using `F`.

```bash
$ ni i"this is how we do it" i"it's friday night" i"and I feel all right" FS
this	is	how	we	do	it
it's	friday	night
and	I	feel	all	right
```

In `ni`, tab-delimited columns of data are referenced like a spreadsheet: the first column is `A`, the second is `B`, etc.
The `f` operator gives you access to the columns of your data. 

```bash
$ ni i"this is how we do it" i"it's friday night" \
     i"and I feel all right" FS fC
how
night
feel
```

You can select multiple columns by providing multiple letters:

```bash
$ ni i"this is how we do it" i"it's friday night" \
     i"and I feel all right" FS fAB
this	is
it's	friday
and	I
```

You can duplicate a column by using its corresponding letter multiple times:

```bash
$ ni i"this is how we do it" i"it's friday night" \
     i"and I feel all right" FS fAAC
this	this	how
it's	it's	night
and	and	feel
```

To select all columns after a particular column, use `f<col>.`

```bash
$ ni i"this is how we do it" i"it's friday night" \
     i"and I feel all right" FS fAD.
this	we	do	it
it's	
and	all	right
```

To select all data between two columns, inclusive, use a dash. ni defaults to
using `cut` to process column selections when possible, which normally produces
identical output to its Perl-hosted colum selector; the only difference is on
most platforms when the input has fewer columns than you're selecting. In that
case `cut` will fail to append blanks for the columns you selected, resulting in
a ragged right edge.

You can force ni to disallow `cut` for a specific operator by prepending a `^{}`
configuration block with the `col/disallow-cut` option. This option is enabled
here because the code below is run as a cross-platform unit test.

```bash
$ ni i"this is how we do it" i"it's friday night" \
     i"and I feel all right" FS ^{col/disallow-cut=1} fB-E
is	how	we	do
friday	night		
I	feel	all	right
```

You can also use `f` to re-order selected columns:

```bash
$ ni i"this is how we do it" i"it's friday night" \
     i"and I feel all right" FS fCBAD
how	is	this	we
night	friday	it's	
feel	I	and	all
```

Columns can also be accessed by using `#<number>`. The same options are available by subsitituting `A => #0`, `B => #1`, `C => #2`, ... `Z => #25`. This syntax also allows you to access columns 26 and beyond.

```bash
$ ni i"this is how we do it" i"it's friday night" \
     i"and I feel all right" FS f#2#1#0#3
how	is	this	we
night	friday	it's	
feel	I	and	all
```

It is also possible to increase readability by inserting commas between specified columns.

```bash
$ ni i"this is how we do it" i"it's friday night" \
     i"and I feel all right" FS fA,#3.
this	we	do	it
it's	
and	all	right
```


Under the hood, `ni` is using either `cut` or Perl to rearrange the columns. `cut` is much faster of the two, but it's only used when the output columns are in the same relative order as the input columns.


### `x`: Exchange columns

`x` is a shorthand for certain column exchange operations. All of these operations can be written with `f`, usually with a higher number of keystrokes.

```bash
$ ni i"Ain't nobody dope as me" \
     i"I'm dressed so fresh, so clean" \
     i"So fresh and so clean, clean" FS
Ain't	nobody	dope	as	me
I'm	dressed	so	fresh,	so	clean
So	fresh	and	so	clean,	clean
```

`x` exchanges the first two columns (same as `fBA.`)

```bash
$ ni i"Ain't nobody dope as me" \
     i"I'm dressed so fresh, so clean" \
     i"So fresh and so clean, clean" FS x
nobody	Ain't	dope	as	me
dressed	I'm	so	fresh,	so	clean
fresh	So	and	so	clean,	clean
```

`x` with a single column exchanges that column with column 1.

```bash
$ ni i"Ain't nobody dope as me" \
     i"I'm dressed so fresh, so clean" \
     i"So fresh and so clean, clean" FS xD
as	nobody	dope	Ain't	me
fresh,	dressed	so	I'm	so	clean
so	fresh	and	So	clean,	clean
```

`x` with mulitple columns exchanges those into the first columns, in order.

```bash
$ ni i"Ain't nobody dope as me" \
     i"I'm dressed so fresh, so clean" \
     i"So fresh and so clean, clean" FS xEB
me	nobody	dope	as	Ain't
so	dressed	so	fresh,	I'm	clean
clean,	fresh	and	so	So	clean
```



## Sort, Unique & Count


### `g`: General sorting
With a single column of data, as in the example, the simple command `g` will give you lexicographic sorting in ascending order. 

```bash
$ ni ib ia ic g
a
b
c
```

To do more complicated sorts, you can give `g` columns (`A-Z`) and modifiers. For example, `-` after a column reverses the sort.

```bash
$ ni ib ia ic gA-
c
b
a
```

`n` after a column does a numeric sort.

```bash
$ ni i10 i5 i0.3 gAn
0.3
5
10
```

You can sort multiple columns in order:


```bash
$ ni i[b 6] i[b 3] i[a 2] i[a 1] i[c 4] i[c 5] i[a 0] gABn
a	0
a	1
a	2
b	3
b	6
c	4
c	5
``` 

The columns can be sorted in any order:

```bash
$ ni i[b 0] i[b 4] i[a 2] i[a 1] i[c 4] i[c 0] i[a 0] gBnA
a	0
b	0
c	0
a	1
a	2
b	4
c	4
```

  
### `o` and `O`: Syntactic Sugar for Numeric Sorting
Often you will want numeric sorting in a more keystroke-efficient way than `gn<column>-`. The `o` (sort rows ascending, numerically) and `O` (sort rows  descending, numerically) operator has been provided for this purpose.

```bash
$ ni i[b 6] i[b 3] i[a 2] i[a 1] i[c 4] i[c 5] i[a 0] oB
a	0
a	1
a	2
b	3
c	4
c	5
b	6
```

```bash
$ ni i[b 6] i[b 3] i[a 2] i[a 1] i[c 4] i[c 5] i[a 0] OB
b	6
c	5
c	4
b	3
a	2
a	1
a	0
```


### `u`: Unique Sorted Rows
`u` is `ni` syntax for `uniq`, which takes sorted rows and returns the unique values.

```bash
$ ni i[b 6] i[b 3] i[a 2] i[a 1] i[c 4] i[c 5] i[a 0] fAgu
a
b
c
```
  
### `c`: Count Sorted Rows
`c` is `ni`'s version of `uniq -c`, which counts the number of identical  consecutive rows in a stream. The main difference is that `ni`'s `c` tab-delimits the output, while `uniq -c` space-delimits.

```bash
$ ni i[b 6] i[b 3] i[a 2] i[a 1] i[c 4] i[c 5] i[a 0] fAgc
3	a
2	b
2	c
```

### `gg`: Grouped sort

Sorts *cannot be chained together using g*. If you write a command like `$ ni ... gA gBn`, there is no guarantee that the output will have a sorted first column after the second sort. If you want to sort by the first column ascending lexicographically and the second column ascending numerically in the same sort, you should use a more explicit `g` operator: `$ni ... gABn`.

If you have data that is already partially sorted, for example, when working with the input of the reduce step of a MapReduce job, you may want to perform an additional sort of the already partially-sorted data without sorting the entire stream. 

Let's simulate this by sorting one column of the data and sinking the result to a tempfile.


```bash
$ ni i[b ba bar] i[b bi bif] i[b ba baz] \
     i[q qa qat] i[q qu quux] i[b ba bake] \
     i[u ub uber] gA \>tmp \<
b	ba	bake
b	ba	bar
b	ba	baz
b	bi	bif
q	qa	qat
q	qu	quux
u	ub	uber
```

If we want the data sorted as if we had done `gAB-`, we cannot simply do `gB-` to the data we have; this will blow away the sort on the first column.

```bash
$ ni i[b ba bar] i[b bi bif] i[b ba baz] \
     i[q qa qat] i[q qu quux] i[b ba bake] \
     i[u ub uber] gA \>tmp \< gB-
u	ub	uber
q	qu	quux
q	qa	qat
b	bi	bif
b	ba	bake
b	ba	bar
b	ba	baz
```

Instead we used the grouped sort `ggAB-`. 

```bash
$ ni i[b ba bar] i[b bi bif] i[b ba baz] \
     i[q qa qat] i[q qu quux] i[b ba bake] \
     i[u ub uber] gA \>tmp \< ggAB-
b	bi	bif
b	ba	bake
b	ba	bar
b	ba	baz
q	qu	quux
q	qa	qat
u	ub	uber
``` 

The first column indicates the key column (i.e. the sorted column we want to hold constant); the anything after the first column is treated like the arguments to the sorting operator, `g`.

### Sorting strategies


Sorting large amounts of data requires buffering to disk. Be careful about how much data you sort; large sorts are a source of headaches and slow performance. 

`$ ni nE7 F// fB gc \>first_numeral_counts.txt`

Running this command you may see see the `ni` [monitor](monitor.md) for the first time, which showing the steps of the computation, and what steps are taking time.  The whole process takes about 2 minutes on my computer.

General sorting is not yet yet a point of strength for `ni`. If your data is larger than a gigabyte uncompressed, you may want to take advantage of massively distributing the workload through Hadoop operations.

## Join and Filter Operators

`ni` has 2 join operators, `j` and `J`. `j` is an inner join simplified functionality similar to the unix `join` command. Like the unix `join`, `j` only works properly when both streams are sorted.

`J` is a left join. it does not impose any requirements on sorting, but requires that the right side of the join fit into memory, and only allows a single value for each key on the right side of the join.

Joining and filtering have a lot in common, since they both require a key. Later in this section, some more uses of `r` to filter datasets are demonstrated.

### `j`: Streaming Inner Join 

You can use the `j` operator to inner-join two streams.
 
```bash
$ ni i[foo bar] i[foo car] i[foo dar] i[that no] i[this yes] j[ i[foo mine] i[not here] i[this OK] i[this yipes] ]
foo	bar	mine
foo	car	mine
foo	dar	mine
this	yes	OK
this	yes	yipes
```

Without any options, `j` will join on the first tab-delimited column of both streams, however, `j` can join on multiple columns by referencing the columns by letter:

```bash
$ ni i[M N foo] i[M N bar] i[M O qux] i[X Y cat] i[X Z dog] \
  jAB[ i[M N hi] i[X Y bye] ]
M	N	foo	hi
M	N	bar	hi
X	Y	cat	bye
```


In general, the streams you are joining should be pre-sorted (though `j` will not fail if the streams aren't sorted).

The join here is slightly *asymmetric*; the left side of the join is streamed, while the right side is buffered into memory. This is useful to keep in mind for joins in a Hadoop Streaming context; the **left** side of the join should be larger (i.e. have more records) than the right side.

### `J`: In-memory (limited) left join


```bash
$ ni i[foo bar] i[foo car] i[that no] i[this yes] i[foo dar] \
     J[ i[this yipes] i[this OK] i[foo mine] i[not here] ]
foo	bar	mine
foo	car	mine
that	no	
this	yes	OK
foo	dar	mine
```

There are a number of things to notice here. First, we can reiterate that neither the right nor the left side of the join need to be sorted. Second, notice that this is a left join--the row `i[that no]` passes through even though there is no associated key on the right. On the right side of the join, notice that the value associated with the **last** element with the same key is used.

### Filtering with `r`

`r` can more generally be thought of as "take rows where the predicate that follows evaluates to true." 


#### `r<col>` Null filtering

Because an empty column is falsey (it evaluates to false), we can filter it using `r`. 

```bash
$ ni i[one_column] i[two columns] i[three columns here] rB
two	columns
three	columns	here
```

#### `ri<col>`: Set Filtering 

A very common motif in `ni` (especially in the MapReduce context) is to filter a large dataset down to a much smaller one with a certain set of keys or values. You can also filter a column based on another dataset using `ri` and the index of the column to filter.

```bash
$ ni i[one_column] i[two columns] i[three columns here] \
     riA[ione_column ithree]
one_column
three	columns	here
```

#### `r/regex/`: regex filtering

In this context, `r` can take a regex as an option: `$ ni <data> r/<regex>/` takes all rows where the regex has a match. We can rewrite our example for `e` `$ ni n500 e'grep 22'` as:

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

To use escaped characters in a regex, it's often more efficient to wrap in quotes:

```bash
$ ni n1000 r-500 r'/^(\d)\1+$/'
555
666
777
888
999
```

To write this same command without quotes requires a lot of escaping: `$ ni n1000 r-500 r/^\(\\d\)\\1+$/`


## `ni` Coding and Debugging

The simplest way to build up a `ni` spell is by writing one step of the spell, checking that step's output for correctness, then writing another step.

In general, `ni` spells will start producing output very quickly (or can be coerced to produce output quickly). Once the output of one step in the spell looks good, you can move on to the next step.


### `--explain`: Print information on command

As you advance through this tutorial, you'll want a quicker way to understand at a high level what a particular `ni` spell is doing. For this, use `ni --explain ...`. Using the example from the file output section:

```bash
$ ni --explain n10 \>ten.txt \<
["n",1,11]
["file_write","ten.txt"]
["file_read"]
```

Each line represents one step of the pipeline defined by the spell, and the explanation shows how the `ni` parser interprets what's written. The explanations are usually concise, but they can help you make sure your code is doing what it's supposed to.


## Conclusion

### Staying in a command-line environment

`ni` is a bottom-up, ad hoc language; `ni` spells can be developed efficiently from the command line, or from a command line-like environment, like a Jupyter notebook.

### Wrap-Up
Congrats on making it to the end of the first part. Hopefully you're starting to see the power in `ni`'s conciseness. If you haven't gotten a chance to develop or play with `ni` code yet, there will likely be some accompanying exercises for this tutorial in the near future, or you can write some yourself and contribute to the development of this fascinating language.

The next chapter covers all the Perl you need to be productive in `ni`. You need some, but not too much.

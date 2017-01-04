#Ni by Example (WIP)

The learning curve for `ni` is steep, but the power curve is exponential.

This tutorial is comprehensive, but not exhaustive. It will introduce you to the philosophy behind `ni`, which is quite different from many of the scripting languages that you have worked with; and give you the tools to explore the rest of `ni`'s rich and wonderful documentation.

In general, this tutorial follows along with the horribly-misnamed `ni` [cheatsheet](cheatsheet.md). If you find this tutorial too slow, you can drink from the firehose there.

##Installation
`ni` should work on any Unix-based OS. If you have Windows and want `ni`, go get Cygwin or VirtualBox or Docker or save yourself the trouble and give your hard drive a good wipe and a fresh Ubuntu install. 

```
git clone git@github.com:spencertipping/ni.git
cd ni
./build
ln -s ni ~/bin/ni  # or whatever to add it to your path
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

**BUG** `ni n0` raises error instead of generating 0-based infinite stream of integers.

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

`r` is a powerful and flexible operation for filtering rows. Here's a short demonstration of its abilities:

* `$ ni n10 r3` - take the first 3 rows of the stream
* `$ ni n10 r-3` - take everything after the first 3 rows of the stream
* `$ ni n10 r~3` - take the last 3 rows of the stream
* `$ ni n10 rx3` - take every 3rd row in the stream
* `$ ni n10 r.15` - sample 15% of the rows in the stream
  * The sampling here is deterministic (conditioned on the environment variable `NI_SEED`) and will always return the first row.

`r` also can take a regex: `$ ni <data> r/<regex>/` takes all rows where the regex has a match.

For example, you can take all of the numbers that end in 0, 1, or 5 with the following spell: `$ ni n20 r/[015]$/`

Because you're typing directly into the bash shell, some characters may need to be escaped, for example in this expression which identifies numbers that are all repeating digits.

`$ ni n1000 r/^\(\\d\)\\1+$/`

Use of escape characters in `ni` operators is acceptable style only if there is not a conciser way that is at least as readable, or a more readable way that is at least as concise. way to do the job. In this case, there is both. Making use of the Perl operator, introduced in the next section allows the above spell to be written more clearly and concisely written as `ni n1000 rp'/^(\d)\1+$/'`

The `r` operator is especially useful during development; for example, if you are working with a large file or stream, you can check the correctness of your output using `r10`, `rx100`, `r.001` etc. to downsample and cap the amount of data read.  With the exception of operators that require processing the entire stream (sorting, for example), **PULL QUOTE all `ni` development can be I/O-bounded, and not processor-bounded, regardless of the resources required by the computation.**


####`\<`: Read from File Names in Stream
When the `\>` file writing operator was introduced, you may have questioned what the purpose of emitting the filename was. The answer is the file reading operator `\<`.

`\<` interprets its input stream as file names, and it will output the contents of these files in order. 

Note that `ni` does not insert newlines between input from separate files. In general, this is a feature, as it allows zip files to be partitioned (for example, in HDFS). However, if you're reading from multiple raw text files, you may need to make sure that the file ends with a newline.

####`|` and `>`: Piping and Redirecting Output

Like other command-line utilities, `ni` respects pipes (`|`) and redirects (`>`). 

####Enrichment: `ni` and directories

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

##Perl Operations
`ni` and Perl (5) go well together philosophically. Both have deeply flawed lexicons and both are highly efficient in terms of developer time and processing time. `ni` and Perl scripts are both difficult to read to the uninitiated. They demand a much higher baseline level of expertise to understand what's going on than, for example, a Python script. 

In addition to Perl, `ni` offers direct interfaces to Ruby and Lisp. While all three of the languages are useful and actively maintained, `ni` is written in Perl, and it is by far the most useful of the three. If you haven't learned Perl yet, but you're familiar with another scripting language, like Python or Ruby, I found [this course]() on Udemy useful for learning Perl's odd syntax.

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

**PULL QUOTE** `ni` is like `python -c` if `python -c` could do more than one useful thing.

`ni` removes all of that; the moment you type `p'...'`, you're thrown directly into the middle of your Perl main subroutine, with `$_` already set implicitly to the incoming line of the input stream.

**FEATURE REQUEST**: `ni` introspection inside `p'...'`--probably a huge pain. And then people would ask you to do `m'...'` and `l'...'`.

####`p'r ...'`: Perl emit row

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

Without `r`, every value separated by a comma is **returned** on its own row; these returned rows are then sent to the output stream.

The `p'r ...` operator, on the other hand, **returns _undefined_** (`undef` in Perl). It works by printing the values separated by columns to a single tab-delimited row of the output stream.

Now that the practical differences between `p'r...'` and `p'...'` have been explained, we can examine the differences in their use that are entailed.  

Clearly, If the desired output of the Perl mapper is two or more columns per row of stream data, you must use `r`. If the desired output of the perl mapper step is a single column per row, you could either use `r` or not.  The more concise statement leaving out `r` is preferred.

When it is clear from context (as above), `p'r...'` is often referred to as `r` for efficiency. This is not the take-rows operator (also called `r`).

####Column Accessor Functions `a` and `a()`

`ni` provides access to all of standard Perl 5, plus a number of functions that significantly increase the keystroke-efficiency and readability of `ni` spells.

The most fundamental of these are the column accessor functions `a(), b(), c(),..., l()`. These functions give access to the values of the first 12 columns of the input data. If you're wondering, the reason that there is no `m` is because it is a reserved character by the Perl system for writing regular expressions with nonstandard delimiters (like pipes or ampersands, etc.)

The functions `a() ... l()` are usually shortened to `a, b, c, ..., l` when their meanings would be unambiguous.  In general this is the case;  the one important exception to this rule is hash lookup, which requires that the user call the function explicitly.
 
Note that these functions do not pollute your namespace, so you can write confusing and pointless `ni` spells like this:

`ni n1 p'my $a = 5; r a, $a'`

If you can understand that, you're well on your way on mastering enough Perl to be proficient in `ni`.

Taking that a step farther, you can overwrite these functions if you want to rough `ni` up a bit. `ni` is pretty resilient (except when it's not); as in the following example.

`ni n1 p'sub a { "RUDE" }; my $a=19; r $a, a, $a, a' p'r a, b, c, d'`

```
Prototype mismatch: sub ni::pl::a () vs none at - line 411.
19      RUDE    19      RUDE
(END)
```

Observe that rewriting `a()` in the first perl mapper had no effect on the functioning of `a` or any of the other column accessors in the second mapper.

**BUG** Should we actually let these functions be overwritten? Can these examples be stretched out to actually break `ni` core functions?

####`rp'...'`: Take rows based on Perl

We can combine the take-rows operator `r` with the Perl operator `p'...'` to create powerful filters. In this case, `r` will take all rows where the output of the Perl statement **is _truthy_ in Perl**.

Because the `p'r...'` operator returns undefined (`undef` in Perl), which is **_truthy_(!)**, `rp'r...'` will act as the identity function on the stream.

Other caveats: the number 0, the string 0 (which is the same as the number 0), and the empty string are all **falsey** in Perl. Pretty much everything else is truthy. Moreover, there is no boolean True or False in Perl, so `false` and `False` are still truthy.

**BUG**: Ask spencer if I'm right about this stuff.

Examples:

* `ni n03 rp'$_' -- returns 1, 2 because the return value of the first row, 0, is falsey
* `ni n03 rp'a'` -- returns 1, 2 because the return value of the first row, 0, is falsey
* `ni n03 rp'r a'` -- returns 0, 1, 2 because the return value of each row is undef, which is truthy.
* `ni n03 rp'b' -- returns nothing because the return value of each row is the empty string, which is falsey.
* `ni n03 rp'r b'` -- returns 5 empty lines **BUG** (i think)
* `ni n03 rp'false'` -- returns 0, 1, 2 because the return value of each row is the string `"false"`, which is truthy.



####Enrichment: `ni` is a quine!

I don't understand this yet, but it's pretty cool, and I think it goes here.


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



##Sort, Unique, and Count
Sorting is often a rate-limiting step in `ni` jobs run on a single machine, and data will need to be buffered to disk if a sort is too large to fit in memory. If your data is larger than a gigabyte uncompressed, you may want to take advantage of massively distributing the workload through Hadoop operations.

* `g`: General sorting
  * `gB` - sort rows ascending by the lexicographic value of the second column
    * Lexicographic value is determined by the ordering of characters in the ASCII table.
    * `ni id:a id:C g` will put the capital `C` before the lower-case `a`, because capital Latin letters precede lowercase Latin letters in ASCII.
  * `gC-` - sort rows *descending* by the lexicographic value of the third column
   * `gCA-` - sort rows first by the lexicographic value of the third column, ascending. For rows with the same value for the third column, sort by *descending* value of the first column.
  * `gDn` - sort rows ascending by the *numerical* value of the fourth column.
    * The numeric sort works on integers and floating-point numbers written as decimals.
    * The numeric sort will **not** work on numbers written in exponential/scientific notation
  * `gEnA-` - sort rows ascending by the numerical value of the fifth column; in the case where values in the fifth column are equal, sort by the lexicographic value of the first column, descending.
* `u`: unique sorted rows
  * `$ ni <data> fACgABu` -- get the lexicographically-sorted unique values from the first and third columns of `<data>`.
* `c`: count sorted rows
  * `$ ni <data> fBgc` -- return the number of times each unique value of the second column occurs in `<data>`
  * Note that the above operation is superior to `$ ni <data> gBfBc` (which will give the same results), since the total amount of data that needs to be sorted is reduced.


##Perl for `ni` fundamentals
Note that whitespace is required after every `p'code'` operator; otherwise ni will assume that everything following your quoted code is also Perl.

* Returning data 
  * `p'r ..., ..., ...'`: Print all comma separated expressions to one tab-delimited row to the stream
  * `p'[..., ..., ...]'`: Return each element of the array as a field in a tab-delimited row to the stream.
* Field selection operations
  * `a`, `a()` through `l` and `l()`: Parsimonious field access
    * `a` through `l` are functions that access the first through twelfth fields of an input data stream. They are syntactic sugar for the functions `a()` through `l()` with the same functionality.
    * Generally, the functions `a` through `l` will be more parsimonious, however, in certain contexts (importantly, this includes hash lookup), these one-letter functions will be interpreted as strings; in this case, you must use the more explicit `a()` syntax. 
  * `F_`: Explicit field access
    * Useful for accessing fields beyond the first 12, for example `$ ni <data> F_ 6..15`
    * `FM` is the number of fields in the row.
    * `FR n` is equivalent to `F_ n..FM`


##Connecting `bash` and `ni`

* `e[<script>]`: Evaluate script
  * evaluate `<script>` in bash, and stream out the results one line at a time.

##Input Operations
* `D:<field1>,:<field2>...`: JSON Destructure
  * `ni` implements a very fast JSON parser that is great at pulling out string and numeral fields.
  * As of 2016-12-24, the JSON destructurer does not support list-based fields in JSON.



##Basic Column Operations
Columns are referenced "Excel-style"--the first column is `A`, the second is `B`, etc.

* `F`: Split stream into columns
  * `F:<char>`: split on character
    * Note: this does not work with certain characters that need to be escaped; use `F/regex/` below for more flexibility (at the cost of less concision).
  * `F/regex/`: split on occurrences of regex. If present, the first capture group will be included before a tab is appended to a field.
  * `Fm/regex/`: don't split; instead, look for matches of regex and use those as
  the field values.
  * `FC`: split on commas (doesn't handle special CSV cases)
  * `FV`: parse CSV "correctly", up to newlines in fields
  * `FS`: split on runs of horizontal whitespace
  * `FW`: split on runs of non-word characters
  * `FP`: split on pipe symbols


  

  
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


##Useful Syntactic Sugar
* `o` and `O`: Numeric sorting
  * `o`: Sort rows ascending (numerical)
    * `oA` is syntactic sugar for `$ ni <data> gAn`
  * `O`: sort rows descending (numerical)
    * `OB` is equivalent to `$ ni <data> gBn-` 
  * **Important Note**: `o` and `O` sorts *cannot be chained together* or combined with `g`. There is no guarantee that the output of `$ ni <data> gAoB` will have a lexicographically sorted first column, and there is no guarantee that `$ ni <data> oBOA` will have a numerically sorted second column.  With very high probability, they will not be sorted.

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
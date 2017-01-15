#`ni` by Example, Chapter 3 (WIP)

Welcome to the third part of the tutorial. At this point, you've probably activated a certain amount of `ni` productivity by using Hadoop to broadcast your `ni` scripts across hundreds of cores and thousands of jobs.  You should also have a reasonable high-level understanding of how `ni` is a self-modifying quine, and how this allows it to execute across operating systems.

In this chapter, our goal is to multiply your power by introducing more I/O operations, important Perl operations including reducers, cell operations, and ni-specific functions. We'll also demonstrate `ni`'s connections to Python and numpy, Ruby, and Lisp. And we'll show how to patch one of `ni`'s weaknesses (large-scale joins) using `nfu`.

But before we explore the breadth of `ni`, we need to return to Perl in a certain amount of depth.

##Perl for `ni`
Perl is much-maligned for its syntax; much of that malignancy comes from people whose only exposure to the language is hearing about the [Obfuscated Perl Contest](https://en.wikipedia.org/wiki/Obfuscated_Perl_Contest). Perl is known for the religious overtones in its construction--let `ni` author Spencer Tipping drop the scales from your eyes in this section from his short intro to the language, [Perl in 10 Minutes](https://github.com/spencertipping/perl-in-ten-minutes). 

>Python, Ruby, and even Javascript were designed to be good languages -- and
just as importantly, to **feel** like good languages. Each embraces the
politically correct notion that values are objects by default, distances itself
from UNIX-as-a-ground-truth, and has a short history that it's willing to
revise or forget. These languages are convenient and inoffensive by principle
because that was the currency that made them viable.
<!--- -->
>Perl is different.
<!--- -->
>In today's world it's a neo-noir character dropped into a Superman comic; but 
that's only true because it changed our collective notion of what an accessible
scripting language should look like. People
often accuse Perl of having no design principles; it's "line noise,"
pragmatic over consistent. This is superficially true, but at a deeper level
Perl is uncompromisingly principled in ways that most other languages aren't.
Perl isn't good; it's complicated, and if you don't know it yet, it will
probably change your idea of what a good language should be.

####Text is numbers

In any nice langauge, strings and numbers are different data types, and trying to use one as another (without an explicit cast) raises an error. Take a look at the following example:

```
$ ni n1p'my $v1="5"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'
15      555     5 golden rings
(END)
```

It's unsurprising that the Perl infix `x` operator (string duplication) and the Perl infix `.` operator (string concatenation) work, but if you come from a friendly language that just wants you to be sure you're doing the right thing, the idea that you can multiply a string by a number and get a number is frustrating. But it gets worse.

```
$ ni n1p'my $v2="4.3" * "6.7"; r $v2'
28.81
(END)
```

You can call floating point multiplication on two string variables with zero consequences.

```
$ ni n1p'my $v1="3.1E17"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'
930000000000000000      3.1E173.1E173.1E17      3.1E17 golden rings
(END)
```

Non-numeric strings are automatically cast 0.

```
$ ni n1p'my $v1="hi"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'
0       hihihi  hi golden rings
(END)
```

Oh, and the converse? NOT TRUE. Text is numbers. Numbers are not text.

####Sigils

So far, we've been using without 

Consider the following example:

```
$xs = 5;
@xs = 1..10;
%xs = (foo => 1, bar => 2);
```

Programmers of inoffensive languages may find the above code shocking and abhorrent (as the author of this text once did). That this code works, and that all of the variables defined within would properly be referred to (at least, in brief) as `xs`, can trigger within a reasonably-skilled but Perl-ignorant programmer, a great deal of cognitive dissonance.

The question here, and its relevance for `ni`, is not whether this language syntax is useful (it is), but how to open our minds and increase our reading skills to take advantage of Perl's unique qualities.

When a Perl variable is brought into existence, its nature is determined by  

Perl variables all start with "sigils" (though sometimes these sigils are implicit). The explicit use of sigils allows for more variables to be packed into the same linguistic namespace. When creating a variable:

* `$` indicates a scalar, like a string or text
* `@` indicates an array
* `%` indicates a hash



####Subroutines

Perl is a multi-paradigm language, but the dominant paradigm for Perl within `ni` is procedural programming. Unlike the object-oriented languages with which you are likely familiar, procedural languages   

Perl subroutines store the variables with which they are called in a default variable named `@_`. Take a moment here to think about how one would refer to the 

####Default Variables
While nice languages make you take pains to indicate default values and variables, Perl is not at all nice in this regard.
`ni` takes advantage


##Intermediate Perl Operations
####`p'^{...} ...'`: Begin Block
A begin block is indicated by attaching a caret (`^`) to a block of code (encolsed in `{ }`). Outside of begin blocks, the Perl code is evaluated for every row; inside a Begin Block, the code is evaluated once and factored over the entire remaining Perl code.

Begin Blocks are useful for initializing data structures that will be manipulated, and in particular for converting data closures to Perl data structures, as we will see Later in this section.

#### `a_` through `l_`: Multiline Selection operations 
You have seen one way to generate multiple lines through the production of data closures. In order to access data from a specific column of the data closure, you will need to use multiline operators `a_` through `l_`, which are the multiline analogs to the line-based operators `a/a()` through `l/l()`.

For example:

* `ni ::data[n5] n1p'a(data)'` and `ni ::data[n5] n1p'a data'` will raise syntax errors, since `a/a()` are not prepared to deal with the more than one line data in the closure.
* `ni ::data[n5] n1p'a_ data'` works, because `a_` operates on each line.


#### `p'%h = <key_col><val_col>_ @lines`: Hash constructor
Hash constructors are useful for filtering large datasets without having to invoke an expensive sort or an HDFS join. The hash constructor is also a useful demonstration of both multiline selection and begin blocks.

* Generate a list of things you want to filter, and put it in a data closure. `::ids[list_of_ids]`
* Convert the data closure to a hash using a begin block (`^{%id_hash = ab_ ids}`)
* Filter another dataset (`ids_and_data`) using the hash (`exists($id_hash{a()})`)
* `ni ::ids[list_of_ids] ids_and_data rp'^{%id_hash = ab_ ids} exists($id_hash{a()})'` 


##Streaming Reduce
So far, you have seen many ways to reshape, process, and filter individual rows, but only one way to summarize multiple rows, the count operator `c`. In this section, we will cover `ni`'s highly flexible, constant-space reduce methods in Perl.

One of the reasons that Perl has been stressed in this tutorial is for streaming reduce; reduction can also be done from within `ni` using Python (introduced in the next section), however, this reduction is limited by a slightly awkward syntax, and the sometimes-unfeasible prospect of needing to hold the entire stream in memory.

Reduction is dependent on the stream being appropriately sorted, which can make the combined act of sort + reduce expensive to execute on a single machine. Operations like these are (unsurprisingly) good options for using in the combiner or reducer steps of `HS` operations.

These operations encapsulate the most common types of reduce operations that you would want to do on a dataset; if your operation is more complicated, it may be more effectively performed using buffered readahead and multi-line reducers.




#### `sr`: Reduce over entire stream

Let's look at a simple example: 

```
$ ni n1E5p'sr {$_[0] + a} 0'
5000050000
(END)
```

The above code sums the integers from 1 to 100,000, inclusive. For those not familiar with Perl syntax, `$_[i]` refers to the i-th argument to a function. `$_[i]` is a dereferencing of the arguments of a function, stored in the magic variable `@_`. Since the variable returned is a scalar, its sigil is a `$`. Confusingly, `$_[i]` has very little to do with `$_`, the Perl default scalar.
 
Outside of Perl's trickiness,that the syntax is relatively simple; `sr` takes an anonymous function wrapped in curly braces, and one or more initial values. A more general syntax is the following: 
 
 ```
($x, $y, ...) = sr {reducer} $x0, $y0, ...
```

To return both the sum of all the integers as well as their product, as well as them all concatenated as a string, we could write the following:

```
$ ni n1E1p'r sr {$_[0] + a, $_[1] * a, $_[2] . a} 0, 1, ""'
55      3628800 12345678910
(END)
```


#### `se`: Reduce while equal

`@final_state = se {reducer} \&partition_fn, @init_state`

####`sea` through `seq`: Reduce with partition function `a()...q()`

#### `rc`: Compound reduce

#### `rfn`: Custom compound reduce



##Buffered Readahead
These operations are good for reducing 

* `rw`: read while
  * `@lines = rw {condition}`: read lines while a condition is met
* `ru`: read until
  * `@lines = ru {condition}`: read lines until a condition is met
* `re`: read equal
  * `@lines = re {condition}`: read lines while the value of the condition is equal.

####Multiline Reducers
These operations can be used to reduce the data output by the readahead functions. Look at the input provided by the first perl statement, 

* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'sum b_ re {a}'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'sum a_ re {b}'`

`rea` is the more commonly used shorthand for `re {a}`

* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r all {a_($_)} reb'`
* `ni n1p'cart ["a", "a", "b", "c"], [1, 2]' p'r uniq a_ reb'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r maxstr a_ reb'`
* `ni n1p'cart ["a", "b", "c"], [1, 2]' p'r reduce {$_ + a} 0, reb` <- DOES NOT WORK BECAUSE BILOW WROTE IT WRONG


##Numpy Operations

Writing Perl reducers is among the most challenging aspects of `ni` development, and it is arguably more error-prone than most other operations because proper reduction depends on an input sort. 

Moreover, Perl reducers are good with data, but they're still not great with math. If you had considered doing matrix multiplication in `ni`, you'd be pretty much out of luck. 

However, `ni` provides 

#### `N'x = ...'`: Numpy matrix operations
  * Dense matrices can be transformed using Numpy-like operations
  * The entire input matrix (i.e. the stream) is referred to as `x`.
  * Example: `ni n10p'r map a*$_, 1..10' N'x = x + 1'` creates a matrix and adds one to every element with high keystroke efficiency.
  * Example `ni n10p'r map a*$_, 1..10' N'x = x.T'`
  
What `ni` is actually doing here is taking the code that you write and inserting it into a Python environment (you can see the python used with `ni e[which python]`

Any legal Python script is allowable, so if you're comfortable with `pandas` you can execute scripts like:

`ni ... N'import pandas as pd; df = pd.DataFrame(x); ... ; x = df.reset_index().values' ...`

The last line is key, because the data that is streamed out of this operation must be stored in the variable `x`.

Also, like other operators, `N'...'` requires at least a row for input. `ni N'x = random.rand(size=(5,5)); x = dot(x, x.T)'` will return nothing, but adding a dummy row `ni n1N'x = random.rand(size=(5,5)); x = dot(x, x.T)'` will.

This is not (yet) the cleanest or most beautiful syntax [and that matters!], but it works.


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

Here's how `ni` solves those:
* Hadoop Streaming
* Numpy Operations
* HDFS Joins



####`ni` is a domain-specific language; its domain is processing single lines and chunks of data that fit in memory

* Because of this philosophy, `ni` is fantastic for data munging and cleaning.
* Because of this philosophy, large-scale sorting is not a `ni`ic operation, while gzip compression is.
* Because of this philosophy, `ni` relies heavily on Hadoop for big data processing. Without Hadoop, most sufficiently complicated operations become infeasible from a runtime perspective once the amount of data exceeds a couple of gigabytes, uncompressed.



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
  



##JSON I/O
*  `p'json_encode {<row to JSON instructions>}`: JSON Encode
  *  The syntax of the row to JSON instructions is difficult; I believe `ni` will try to interpret value as a `ni` command, but every other unquoted piece of text will be interpreted as 
  *  Here's an example:
  
```
ni //license FWpF_ p'r pl 3' \
     p'json_encode {type    => 'trigram',
                    context => {w1 => a, w2 => b},
                    word    => c}' \>jsons
```

* `D:<field1>,:<field2>...`: JSON Destructure
  * `ni` implements a very fast JSON parser that is great at pulling out string and numeral fields.
  * As of 2016-12-24, the JSON destructurer does not support list-based fields in JSON.

##Directory I/O


Start by making some data:

```
$ rm -rf dir_test && mkdir dir_test
$ echo "hello" > dir_test/hi
$ echo "you" > dir_test/there
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
  
  

##Things other than Perl 

Look, these are here, and if it helps you get started with `ni`, great. But `ni` is written in Perl, for Perl, and in a Perlic style. Use these, but go learn Perl.

*  `m'<...>'`: Ruby
   * applies the Ruby snippet `<...>` to each row of the stream 
*  `l'<...>'`: Lisp
   * applies the Lisp snippet `<...>` to each row of the stream 
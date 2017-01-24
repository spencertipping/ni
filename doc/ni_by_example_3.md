#`ni` by Example, Chapter 3 (alpha release)

Welcome to the third part of the tutorial. At this point, you've probably activated a certain amount of `ni` productivity by using Hadoop to broadcast your `ni` scripts across hundreds of cores and thousands of jobs.  You should also have a reasonable high-level understanding of how `ni` is a self-modifying quine, and how this allows it to execute across operating systems.

In this chapter, our goal is to multiply your power by introducing reducers. We'll also discuss some more common I/O operations, give an overview of Perl fundamentals, and demonstrate `ni`'s connections to Python and numpy. 

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

###Text is numbers

In any nice langauge, strings and numbers are different data types, and trying to use one as another (without an explicit cast) raises an error. Take a look at the following example:

```
$ ni 1p'my $v1="5"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'
15      555     5 golden rings
(END)
```

It's unsurprising that the Perl infix `x` operator (string duplication) and the Perl infix `.` operator (string concatenation) work, but if you come from a friendly language that just wants you to be sure you're doing the right thing, the idea that you can multiply a string by a number and get a number is frustrating. But it gets worse.

```
$ ni 1p'my $v2="4.3" * "6.7"; r $v2'
28.81
(END)
```

You can call floating point multiplication on two string variables with zero consequences.  This is complicated, but not ambiguous; if it is possible to cast the two strings (silently) to numbers, then Perl will do that for you automatically. Strings that start with valid numbers are cast to the longest  component parseable as a float or integer, and strings that do not are cast to zero in a numeric context.

```
$ ni 1p'my $v1="hi"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'
0       hihihi  hi golden rings
(END)
```

```
$ ni 1p'my $v1="3.14hi"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'
9.42    3.14hi3.14hi3.14hi      3.14hi golden rings
(END)
```

```
$ ni 1p'my $v1="3.1E17"; r $v1 * 3, $v1 x 3, $v1 . " golden rings"'
930000000000000000      3.1E173.1E173.1E17      3.1E17 golden rings
(END)
```


###Sigils

So far, we've been using without explanation Perl variables that start with the character `$`. This character is referred to as a sigil, and is not a part of the variable name.  Sigils are used in different ways by the Perl interpreter to increase the language's concision.


```
$x = 5;
@x = 1..10;
%x = (foo => 1, bar => 2);
```

Programmers of inoffensive languages may find the above code shocking and abhorrent (as the author of this text once did). That this code works, and that all of the variables defined within would properly be referred to (at least, in brief) as `x`, can trigger a great deal of cognitive dissonance within a reasonably-skilled but Perl-ignorant programmer.

The question here, and its relevance for `ni`, is not whether this language syntax is useful (it is), but how to open our minds and increase our reading skills to take advantage of Perl's unique qualities.

When a Perl variable is brought into existence, its nature is determined by the sigil that precedes it. The explicit use of sigils allows for more variables to be packed into the same linguistic namespace. When creating a variable:

* `my $x` indicates that `$x` is a scalar, for example a string or a number
* `my @x` indicates that `@x` is an array
* `my %x` indicates that `%x` is a hash

What's even cooler is that, because the syntax for all of these is different, we can use all of these:

```
print $x;       # gets the scalar value of $x
print $x[3];    # gets a scalar value from @x
print $x{foo};  # gets a scalar value from %x
```

At first glance, this is very confusing; all of these values start with `$x`--but note that the calling syntax is different for all three; you get a scalar value (i.e `$`) out of a hash by calling it with curly braces, you get a scalar value out of an array by calling it with square brackets, and without either of those, Perl knows that you are referring to the scalar value `$x`. The syntax is a little complicated, but it's not tricky.

###Barewords are strings
Consider the following `ni` spell:

```
$ ni n3p'r a, one'
1       one
2       one
3       one
(END)
```

Whereas in almost any other language, a syntax error or name error would be raised on referencing a variable that does not exist,  Perl gives the programmer a great deal of freedom to be concise. The bareword (a Perl term for a variable not prefixed with a sigil) `one` has not been defined, so Perl assumes you know what you're doing and interprets it as a string. Perl assumes you are a great programmer, and in doing so, allows you to rise to the challenge.


###Subroutines

Perl is a multi-paradigm language, but the dominant paradigm for Perl within `ni` is procedural programming. Unlike the object-oriented languages with which you are likely familiar, where methods are objects (often "first-class" objects, [whatever that means](http://stackoverflow.com/questions/245192/what-are-first-class-objects)) procedural languages focus on their functions, called "subroutines."  

Perl subroutines store the variables with which they are called in a default variable named `@_`. Before continuing, take a moment to think about how one would refer the elements in `@_`.

Subroutines are stored and referenced with the following syntax:

```
*x = sub {"hi"}
$v = &x         # $v will be set to "hi"
```


Here's how you'd write a function that copies a stream of values 4 times in `ni`, using Perl.

```
$ ni n3p'*v = sub {$_[0] x 4}; &v(a)'
1111
2222
3333
(END)
```

To review the syntax, the *name* of the variable is `_`, and within the body of the subroutine, the array associated with that name, `@_` is the array of values that are passed to the function.  To get any particular scalar value within `@_`, you tell Perl you want a scalar (`$`) from the variable with name `_`, and then indicate to Perl that of the variables named `_`, you want to reference the array, by using the postfix `[0]`.

Subroutines can also be called without the preceding `&`, or created using the following syntax:

```
$ ni 1p'sub yo {"hi " . $_[0]} yo a'
hi 1
(END)
```

Note that in these examples, a new function will be defined for every line in the input, which is highly inefficient. In the next section, we will introduce begin blocks, which allow functions to be defined once and then used over all of the lines in a Perl mapper block.

###Default Variables
While nice languages make you take pains to indicate default values and variables, Perl is not at all nice in this regard.

Consider the following spell:

```
$ ni n20p'/^(\d)\d*$/'
```

It looks like the Perl snippet is using a regular expression to take the first digit of a number. What's not clear is which number it's using. Looking at the output:

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
1
1
1
1
1
1
1
1
1
1
2
(END)
```

Clearly this regex is operating on each line (the lines input were the integers 1 through 20). The way this works is another piece of Perl's uncompromising commitment to coding efficiency; default variables.

In fact, the code above is operating on the most important of the Perl default variables, `$_`, which stores the value of the input line. Note that it shares the same name as the variable `@_`, and the similar-looking `$_[...]` will reference one of the scalars in `@_`, and not one of the characters in `$_`. Python-style string slicing (which uses square brackets) is not one available in vanilla Perl 5, where the `substr` method (or regexes) are more commonly used.


##Intermediate Perl Operations
###`p'^{...} ...'`: BEGIN Block
A begin block is indicated by attaching a caret (`^`) to a block of code (enclosed in `{ }`). Begin blocks are most useful for initializing data structures that will be manipulated, and in particular for converting data closures to Perl data structures, as we will see later in this section.

Inside a begin block, the code is evaluated once and factored over the entire remaining Perl code. Here is a contrived example based on the previous section:

```
$ ni n3p'*v = sub {$_[0] x 4}; &v(a)'
1111
2222
3333
(END)
```

In this example, `*v` is being computed at runtime for each row. Using a begin block, however, `*v` will be computed just once, so this could be better written as:

```
$ ni n3p'^{*v = sub {$_[0] x 4}} &v(a)'
1111
2222
3333
(END)
```

However useful to exemplify the concept of a begin block, it turns out that all of the previous definitions are not practical. Outside of variable assignment, `sub` is a `BEGIN`-level construct, so the code is much more simply written as:

```
$ ni n3p'sub v {$_[0] x 4} &v(a)'
1111
2222
3333
(END)
```


### `a_` through `l_`: Multiline Selection operations 
You have seen one way to generate multiple lines through the production of data closures. In order to access data from a specific column of the data closure, you will need to use multiline operators `a_` through `l_`, which are the multiline analogs to the line-based operators `a/a()` through `l/l()`.

For example:

* `$ ni ::data[n5] 1p'a(data)'` and `$ ni ::data[n5] 1p'a data'` will raise syntax errors, since `a/a()` are not prepared to deal with the more than one line data in the closure.
* `$ ni ::data[n5] 1p'a_ data'` works, because `a_` operates on each line.


### `p'%h = <key_col><val_col>_ @lines`: Hash constructor
Hash constructors are useful for filtering large datasets without having to invoke an expensive sort or an HDFS join. The hash constructor is also a useful demonstration of both multiline selection and begin blocks.

* Generate a list of things you want to filter, and put it in a data closure. `::ids[list_of_ids]`
* Convert the data closure to a hash using a begin block (`^{%id_hash = ab_ ids}`)
* Filter another dataset (`ids_and_data`) using the hash (`exists($id_hash{a()})`)
* `$ ni ::ids[list_of_ids] ids_and_data rp'^{%id_hash = ab_ ids} exists($id_hash{a()})'` 


##Streaming Reduce
So far, you have seen many ways to reshape, process, and filter individual rows, but only one way to summarize multiple rows, the count operator `c`. In this section, we will cover `ni`'s highly flexible, constant-space reduce methods in Perl.

One of the reasons that Perl has been stressed in this tutorial is for streaming reduce; reduction can also be done from within `ni` using Python (introduced in the next section), however, this reduction is limited by a slightly awkward syntax, and the sometimes-unfeasible prospect of needing to hold the entire stream in memory.

Reduction is dependent on the stream being appropriately sorted, which can make the combined act of sort and reduce expensive to execute on a single machine. Operations like these are (unsurprisingly) good options for using in the combiner or reducer steps of `HS` operations.

These operations encapsulate the most common types of reduce operations that you would want to do on a dataset; if your operation is more complicated, it may be more effectively performed using buffered readahead and multi-line reducers.

### `sr`: Reduce over entire stream

Let's look at a simple example, summing the integers from 1 to 100,000: 

```
$ ni n1E5p'sr {$_[0] + a} 0'
5000050000
(END)
```

Outside of Perl's trickiness,that the syntax is relatively simple; `sr` takes an anonymous function wrapped in curly braces, and one or more initial values. A more general syntax is the following: 
 
 ```
@final_state = sr {reducer} @init_state
```

To return both the sum of all the integers as well as their product, as well as them all concatenated as a string, we could write the following:

```
$ ni n1E1p'r sr {$_[0] + a, $_[1] * a, $_[2] . a} 0, 1, ""'
55      3628800 12345678910
(END)
```
A few useful details to note here: The array `@init_state` is read automatically from the comma-separated list of arguments; it does not need to be wrapped in parentheses like one would use to explicitly declare an array in Perl. The results of this streaming reduce come out on separate lines, which is how `p'...'` returns from array-valued functions


### `se`: Reduce while equal
`sr` is useful, but limited in that it will always reduce the entire stream. Often, it is more useful to reduce over a specific set of criteria.

Let's say we want to sum all of the 1-digit numbers, all of the 2-digit numbers, all of the 3-digit numbers, etc. The first thing to check is that our input is sorted, and we're in luck, because when we use the `n` operator to generate numbers for us, they'll come out sorted numerically, which implies they will be sorted by their number of digits.

What we'd like to do is, each time a number streams in, to check if it  number of digits is equal to the number of digits for the previous number we saw in the stream. If it does, we'll add it to a running total, otherwise, we emit the running total to the output stream and start a new running total.

In `ni`-speak, the reduce-while-equal operator looks like this:

```
@final_state = se {reducer} \&partition_fn, @init_state
```

`se` differs from `sr` only in the addition of the somewhat cryptic `\&partition_fn`. This is an anonymous function, which is can be expressed pretty much a block with the perl keyword `sub` in front of it. For our example, we'll use `sub {length}`, which uses the implicit default variable `$_`, as our partition function.

**NOTE**: The comma after `partition_fn` is important.

```
$ ni n1000p'se {$_[0] + a} sub {length}, 0'
45
4905
494550
1000
(END)
```

That's pretty good, but it's often useful to know what the value of the partition function was for each partition (this is useful, for example, to catch errors where the input was not sorted). This allows us to see how `se` interacts with row-based operators.

```
$ ni n1000p'r length a, se {$_[0] + a} sub {length}, 0'
1       45
2       4905
3       494550
4       1000
(END)
```

One might worry that the row operators will continue to fire for each line of the input while the streaming reduce operator is running. In fact, the streaming reduce will eat all of the lines, and the first line will only be used once.

If your partition function is sufficiently complicated, you may want to write it once and reference it in multiple places.

```
$ ni n1000p'^{sub len($) {length $_[0]}} r len a, se {$_[0] + a} \&len, 0'
1       45
2       4905
3       494550
4       1000
(END)
```

The code above uses a Perl function signature `sub len($)` tells Perl that the function has one argument. The signature for a function with two arguments would be written as `sub foo($$)`. This allows the function to be called as `len a` rather than the more explicit `len(a)`. The above code will work with or without the begin block syntax.


###`sea` through `seq`: Reduce with partition function `a()...q()`

It's very common that you will want to reduce over one of the columns in the data. For example, we could equivalently use the following spell to perform the same sum-over-number-of digits as we did above. 

```
$ ni n1000p'r a, length a' p'r b, se {$_[0] + a} \&b, 0'
1       45
2       4905
3       494550
4       1000
(END)
```

`ni` offers a shorthand for reducing over a particular column:

```
ni n1000p'r a, length a' p'r b, seb {$_[0] + a} 0'
1       45
2       4905
3       494550
4       1000
(END)
```


### `rc`: Compound reduce

Consider the following reduction operation, which computes the sum, mean, min and max.

```
$ ni n100p'my ($sum, $n, $min, $max) = sr {$_[0] + a, $_[1] + 1,
                                            min($_[2], a), max($_[3], a)}
                                           0, 0, a, a;
            r $sum, $sum / $n, $min, $max'
```

For common operations like these, `ni` offers a shorthand:

```
ni n100p'r rc \&sr, rsum "a", rmean "a", rmin "a", rmax "a"'
```

##Buffered Readahead
These operations are used to convert columnar data into rows, which can then be reduced over. In general, the types of reductions that can be done with buffered readahead and multiline reducers can also be done with streaming reduce; however, the syntax is often more efficient

* `rw`: read while
  * `@lines = rw {condition}`: read lines while a condition is met
* `ru`: read until
  * `@lines = ru {condition}`: read lines until a condition is met
* `re`: read equal
  * `@lines = re {condition}`: read lines while the value of the condition is equal.

###`cart`: Cartesian Product
To generate examples for our buffered readahead, we'll use the builtin `ni` operation `cart`.

```
$ ni 1p'cart [1, 2], ["a", "b", "c"]'
1       a
2       a
1       b
2       b
1       c
2       c
(END)
```

Note that `cart` takeas array references (in square brackets), and returns array references. `ni` will interpret these array references as rows, and expand them. Thus `r`, when applied to `cart`, will likely not produce your desired results.

```
$ ni 1p'r cart [1], ["a", "b", "c"]'
ARRAY(0x7ff2bb109568)   ARRAY(0x7ff2ba8c93c8)   ARRAY(0x7ff2bb109b80)
(END)
```




###Multiline Reducers

```
$ ni 1p'cart [1, 2], ["a", "b", "c"]' p'sum a_ re {b}'
3
3
3
(END)
```

`reb` is the more commonly used shorthand for `re {b}`.

```
$ ni 1p'cart [1, 2], ["a", "b", "c"]' p'sum a_ reb'
3
3
3
(END)
```

There are a lot of other options for you to work with--see [the Perl docs](perl.md) or the [cheatsheet](cheatsheet.md) for more details.

* `ni 1p'cart ["a", "b", "c"], [1, 2]' p'r all {a_($_)} reb'`
* `ni 1p'cart ["a", "a", "b", "c"], [1, 2]' p'r uniq a_ reb'`
* `ni 1p'cart ["a", "b", "c"], [1, 2]' p'r maxstr a_ reb'`
* `ni 1p'cart ["a", "b", "c"], [1, 2]' p'r {$_[0] . a} "", reb'` 


##Numpy Operations

Writing Perl reducers is among the most challenging aspects of `ni` development, and it is arguably more error-prone than most other operations because proper reduction depends on an input sort. 

Moreover, Perl reducers are good with data, but they're still not great with math. If you had considered doing matrix multiplication in `ni` up to this point, you'd be pretty much out of luck. 

However, `ni` provides an interface to numpy (and all of the other Python packages on your machine), which gives you access to hugely powerful mathematical operations.  There are several associated costs, however: 1) the entire stream must be buffered into memory; 2) there are some arbitrary-feeling limitations on I/O; 3) the syntax is clunky compared to Perl.

It's great, you're gonna love it.

### `N'x = ...'`: Numpy matrix operations

The stream input to `N'...'` is converted into a matrix, where each row and column of the input is converted to a corresponding cell in a numpy matrix, `x`.

The values streamed out of `N'...'` are the values of `x`, so all operations that you want to do to the stream must be saved back into `x`. Compared to the Perl syntax, this is inelegant, and if `ni`'s gotten into your soul yet, it should make you more than a little frustrated.

However, the gains in power are quickly manifested:


```
$ ni n3p'r map a*$_, 1..3' N'x = x + 1'
2       3       4
3       5       7
4       7       10
(END)
```

```
$ ni n5p'r map a . $_, 1..3' N'x = x.T'
11      21      31      41      51
12      22      32      42      52
13      23      33      43      53
(END)
```

```
$ ni n1N'x = random.normal(size=(4,3))'
-0.144392928715457      0.823863130371182       -0.0884075304437077
-0.696189074356781      1.5246371050062 -2.33198542804912
1.40260347893123        0.0910618083600519      0.851396708020142
0.52419501996823        -0.546343207826548      -1.67995253555456
(END)
```

```
$ ni i[1 0] i[1 1] N'x = dot(x, x.T)'
1       1
1       2
(END)
```



###How `N'x = ...'` works
What `ni` is actually doing here is taking the code that you write and inserting it into a Python environment (you can see the python used with `ni e[which python]`

Any legal Python script is allowable, so if you're comfortable with `pandas` and you have it installed, you can execute scripts like:

`ni ... N'import pandas as pd; df = pd.DataFrame(x); ... ; df.to_excel(...); x = df.reset_index().values' ...`

The last line is key, because the data that is streamed out of this operation must be stored in the variable `x`.

Also, like other operators, `N'...'` requires at least a row for input. `ni N'x = random.rand(size=(5,5)); x = dot(x, x.T)'` will return nothing, but adding a dummy row `ni n1N'x = random.rand(size=(5,5)); x = dot(x, x.T)'` will.

This is not (yet) the cleanest or most beautiful syntax [and that matters!], but it works.


##JSON I/O

We'll spend the rest of this chapter discussing JSON and directory I/O; the former is fundamental to a lot of the types of operations that `ni` is good at, the latter will expand your understanding of `ni`'s internal workings.

###`D:<field1>,:<field2>...`: JSON Destructure

`ni` implements a very fast JSON parser that is great at pulling out string and numeric fields.  As of this writing (2017-01-16), the JSON destructurer does not support list-based fields in JSON.

###`p'json_encode {<row to JSON instructions>}`: JSON Encode

The syntax of the row to JSON instructions is similar to hash construction syntax in Ruby. 
  
```
ni //license FWpF_ p'r pl 3' \
     p'json_encode {type    => 'trigram',
                    context => {w1 => a, w2 => b},
                    word    => c}' =\>jsons \
     D:type,:word
```
###Other JSON parsing methods
Some aspects of JSON parsing are not quite there yet, so you may want to use (also very fast) raw Perl to destructure your JSONs.



##Directory I/O
The contents of this section 

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

##`ni` Philosophy and Style

If you've made it this far in the tutorial, you now have enough tools to be extremely productive in `ni`. If you're ready to get off the crazy ride of this tutorial and get to work, here's a great point to stop. Before you go, though, it will help to take a few minutes to think through `ni`'s philosophy and style, and how those two intersect.

###`ni` optimizes at-will programmer productivity
Many data science projects start with a request that you have never seen before; in this case, it's often easier to start from very little and be able to build rapidly, than it is to have a huge personal library of Python scripts and Jupyter notebooks that you can bend to fit the task at hand.

`ni` is great at generating throwaway code, and this code can be made production-ready through `ni` scripting, discussed in the next chapter.

###Mastery counts

Just because you can read a Python script and understand what it does at a basic level does not mean you can code in Python, and Python can trick very intelligent people into thinking they know what they're doing even when they don't. The opposite is true of `ni`. It will be inherently obvious when you don't know something in `ni`, because if you don't know something, it'll be likely that the part of the spell you're reading won't make sense.

This is a gruff approach to programming, but it's not unfriendly. `ni` doesn't allow you to just get by--your only option is mastering `ni` one piece at a time.

###Outsource hard jobs to more appropriate tools.

`ni` is a domain-specific language; its domain is processing single lines and chunks of data that fit in memory

* Because of this philosophy, `ni` is fantastic for data munging and cleaning.
* Because of this philosophy, large-scale sorting is not a `ni`ic operation, while gzip compression is.
* Because of this philosophy, `ni` relies heavily on Hadoop for big data processing. Without Hadoop, most sufficiently complicated operations become infeasible from a runtime perspective once the amount of data exceeds a couple of gigabytes uncompressed.

Some jobs that are difficult for `ni`, and some ways to resolve them:

* Sorting
  * Challenge: Requires buffering of entire stream (possibly to disk, which is slow)
  * Solution: Hadoop Streaming will do much of the heavy lifting for you in any sort, and will distribute the computation so it's easy.
* Matrix Multiplication
  * Challenge: Difficult to implement in a streaming context
  * Solution: Numpy operations 
* SQL Joins
  * Challenge: SQL joins can take more than one line
  * Solution:
    * Small Data: There are several options here. You can use `N'...'` and do the join in numpy, you can use `N'...'` with `import pandas as pd` to do the join in pandas, or you can pipe data in and out of `join -t $'\t'`
    * Large Data: HDFS Joins
* Iterating a `ni` process over directories where the directory provides contextual information about its contents.
  * Challenge: This is something `ni` can probably do, but I'm not sure how to do it offhand.
  * Solution: Write out the `ni` spell for the critical part, embed the spell in a script written in Ruby/Python, and call it using `bash -c`.




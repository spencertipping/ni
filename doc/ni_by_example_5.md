#Future Chapter 5 Below


## Understanding the `ni` monitor

At this point, you've probably executed a long-running enough `ni` job to see the `ni` monitor appear at the top of your screen. 

* Negative numbers = non-rate-determining step
* Positive numbers = rate-determining step
* Large Positive numbers = slow step

Some things to keep in mind when examining the output of the monitor: if you have access to more compute resources for slow steps, you can use them for that step alone, and this can be done in a very simple way via the `S` horizontal scaling operator.

You may also want to consider refactoring your job to make use of Hadoop Streaming with `HS`, depending on what's suited to your job. More details are available in the [monitor docs](monitor.md) and in the [optimization docs](optimization.md).


## Even More Perl for Ni

### Other syntaxes for `map`


`map` can also take an expression rather than a block, but this syntax is tricky 


The following example is taken from [Perl Monks](http://www.perlmonks.org/?node_id=613280)

You'd probably be even more surprised by the result of this:
my @in = qw(aaa bbb ccc);
my @out = map { s/.$/x/ } @in;
print "@in\n=>\n@out\n";
[download]
which prints:
aax bbx ccx
=>
1 1 1
[download]
Yeah, the original is modified. Basically: never modify $_ in a map, and remember that the "output" of s/// is the success count/code. To get what you want, you need to localize $_ and reuse it as the last expression evaluated in the block:
my @in = qw(aaa bbb ccc);
my @out = map { local $_ = $_; s/.$/x/; $_ } @in;
print "@in\n=>\n@out\n";
[download]
which indeed shows:
aaa bbb ccc
=>
aax bbx ccx


### `push`, `pop`, `shift`, `unshift`

### `p'... END { }'`: END Block

Similar to a BEGIN block, and END block is used to calculate totals from the data that has accumulated in persistent variables.

```bash
$ ni n5p'^{@x}; push @x, 2*a; undef; END{r join " and ", @x}'
2 and 4 and 6 and 8 and 10
```

We accumulate all of the values in `x`, then join them together and return them all. The statement `undef` is added to make the perl mapper be quiet while it is accumulating values (otherwise, the return value of `push` is the length of the output array).

### `our` and `local`



### `use strict` and the `::` prefix within `p'...'`

When `use strict` is enabled, Perl will complain when you try to create a variable in a Perl snippet that does not start with `::`.

The reasons for this are very specific to Perl; if you are a true Perl nerd, you can look them up, but you do not need to know them if you just accept that variables need to start with `::` when you apply `use strict`. 

It is probably a good idea to `use strict` when the variables you define are sufficiently complex; otherwise you're probably okay not using it.


### Subroutines

Perl is a multi-paradigm language, but the dominant paradigm for Perl within `ni` is procedural programming. Unlike the object-oriented languages with which you are likely familiar, where methods are objects (often "first-class" objects, [whatever that means](http://stackoverflow.com/questions/245192/what-are-first-class-objects)) procedural languages focus on their functions, called "subroutines."  

Perl subroutines store the variables with which they are called in a default variable named `@_`. Before continuing, take a moment to think about how one would refer the elements in `@_`.

Subroutines are stored and referenced with the following syntax:

```
*x = sub {"hi"}
$v = &x         # $v will be set to "hi"
```


Here's how you'd write a function that copies a stream of values 4 times in `ni`, using Perl.

```bash
$ ni n3p'*v = sub {$_[0] x 4}; &v(a)'
1111
2222
3333
```

To review the syntax, the *name* of the variable is `_`, and within the body of the subroutine, the array associated with that name, `@_` is the array of values that are passed to the function.  To get any particular scalar value within `@_`, you tell Perl you want a scalar (`$`) from the variable with name `_`, and then indicate to Perl that of the variables named `_`, you want to reference the array, by using the postfix `[0]`.

Subroutines can also be called without the preceding `&`, or created using the following syntax:

```bash
$ ni 1p'sub yo {"hi " . $_[0]} yo a'
hi 1
```

Note that in these examples, a new function will be defined for every line in the input, which is highly inefficient. In the next section, we will introduce begin blocks, which allow functions to be defined once and then used over all of the lines in a Perl mapper block.

## Streaming Reduce
In earlier drafts of `ni` by example, streaming reduce had a much greater role; however, with time I've found that most operations that are done with streaming reduce are more simply performed using buffered readahead; moreover, buffered readahead.  Streaming reduce is still useful and very much worth knowing as an alternative, highly flexible, constant-space set of reduce methods in Perl.

Reduction is dependent on the stream being appropriately sorted, which can make the combined act of sort and reduce expensive to execute on a single machine. Operations like these are (unsurprisingly) good options for using in the combiner or reducer steps of `HS` operations.

These operations encapsulate the most common types of reduce operations that you would want to do on a dataset; if your operation is more complicated, it may be more effectively performed using buffered readahead and multi-line reducers.

### `sr`: Reduce over entire stream

Let's look at a simple example, summing the integers from 1 to 100,000: 

```bash
$ ni n1E5p'sr {$_[0] + a} 0'
5000050000
```

Outside of Perl's trickiness,that the syntax is relatively simple; `sr` takes an anonymous function wrapped in curly braces, and one or more initial values. A more general syntax is the following: 
 
 ```
@final_state = sr {reducer} @init_state
```

To return both the sum of all the integers as well as their product, as well as them all concatenated as a string, we could write the following:

```bash
$ ni n1E1p'r sr {$_[0] + a, $_[1] * a, $_[2] . a} 0, 1, ""'
55	3628800	12345678910
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

```bash
$ ni n1000p'se {$_[0] + a} sub {length}, 0'
45
4905
494550
1000
```

That's pretty good, but it's often useful to know what the value of the partition function was for each partition (this is useful, for example, to catch errors where the input was not sorted). This allows us to see how `se` interacts with row-based operators.

```bash
$ ni n1000p'r length a, se {$_[0] + a} sub {length}, 0'
1	45
2	4905
3	494550
4	1000
```

One might worry that the row operators will continue to fire for each line of the input while the streaming reduce operator is running. In fact, the streaming reduce will eat all of the lines, and the first line will only be used once.

If your partition function is sufficiently complicated, you may want to write it once and reference it in multiple places.

```bash
$ ni n1000p'sub len($) {length $_}; r len a, se {$_[0] + a} \&len, 0'
1	45
2	4905
3	494550
4	1000
```

The code above uses a Perl function signature `sub len($)` tells Perl that the function has one argument. The signature for a function with two arguments would be written as `sub foo($$)`. This allows the function to be called as `len a` rather than the more explicit `len(a)`. The above code will work with or without the begin block syntax.


### `sea` through `seq`: Reduce with partition function `a()...q()`

It's very common that you will want to reduce over one of the columns in the data. For example, we could equivalently use the following spell to perform the same sum-over-number-of digits as we did above. 

```bash
$ ni n1000p'r a, length a' p'r b, se {$_[0] + a} \&b, 0'
1	45
2	4905
3	494550
4	1000
```

`ni` offers a shorthand for reducing over a particular column and everything to
its left:

```bash
$ ni n1000p'r length a, a' p'r a, sea {$_[0] + b} 0'
1	45
2	4905
3	494550
4	1000
```


### `rc`: Compound reduce

Consider the following reduction operation, which computes the sum, mean, min and max.

```bash
$ ni n100p'my ($sum, $n, $min, $max) = sr {$_[0] + a, $_[1] + 1,
                                            min($_[2], a), max($_[3], a)}
                                           0, 0, a, a;
            r $sum, $sum / $n, $min, $max'
5050	50.5	1	100
```

For common operations like these, `ni` offers a shorthand:

```bash
$ ni n100p'r rc \&sr, rsum "a", rmean "a", rmin "a", rmax "a"'
5050	50.5	1	100
```



## Data Development with `ni` and Hadoop

If you screw up your partitioning, you're ruined. 

JSON => Raw TSV => Cleaned TSV => Filtered TSV => Joined TSV => Statistics

If there's a potential problem with a column, you cannot use it as the first column in a reduce. If you do, Hadoop is very unforgiving; you'll end up having to bail on a particular slice of your data, or restart from the last nearly-equally-partitioned step.

Let's say you have data with 3 columns: `user_id`, `geohash`, `timestamp`. 

It's likely that both some of the users and some of the geohashes are spurious.  This will cause us difficulty down the line, for example when we want to join data to each record using geohash as the joining key.

### `:checkpoint_name[...]`: Checkpoints

Developing and deploying to producion `ni` pipelines that involve multiple Hadoop streaming steps involves a risk of failure outside the programmer's control; when this happens, we don't want to have to run the entire job again. 

Checkpoints allow you to save the results of difficult jobs while continuing to develop in `ni`.

Checkpoints and files share many commonalities. The key difference between a checkpoint and a file are:

1. A checkpoint will wait for an `EOF` before writing to the specified checkpoint name, so the data in a checkpoint file will consist of all the output of the operator it encloses. A file, on the other hand, will accept all output that is streamed in, with no guarantee that it is complete.
2. If you run a `ni` pipeline with checkpoints, and there are files that correspond to the checkpoint names in the directory from which `ni` is being run, then `ni` will use the data in the checkpoints in place of running the job. This allows you to save the work of long and expensive jobs, and to run these jobs multiple times without worrying that bhe steps earlier in the pipeline that have succeeded will have to be re-run.

An example of checkpoint use is the following:

```bash
$ rm -f numbers                 # prevent ni from reusing any existing file
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


### Developing Hadoop Streaming pipelines with checkpoints

As of now, `ni` auto-generates the names for the Hadoop directories, and these can be hard to remember off the top of your head. If you want to 


### Tools for Data Development

1. A good markdown editor; I like Laverna, and it should work on basically all platforms.
2. Infinite patience
3. A reasonable test set.


## Plotting with `ni --js`
Check out the [tutorial](tutorial.md) for some examples of cool, interactive `ni` plotting.

**TODO**: Say something useful.


##Custom Compound Reduce
#### `rfn`: Custom compound reduce

**TODO: Understand this**

## Partitioned Matrix Operations

Operations on huge matrices are not entirely `ni`ic, since they may require space greater than memory, whichwill make them slow. However, operators are provided to improve These operations are suited best to 


* `X<col>`, `Y<col>`, `N<col>`: Matrix Partitioning
  * **TODO**: understand how these actually work.
* `X`: sparse-to-dense transformation
  * In the case that there are collisions for locations `X`, `X` will sum the values
  * For example: `ni n010p'r 0, a%3, 1' X`

## Disk-Backed Data Closures

* `@:[disk_backed_data_closure]`

## Binary Operations
In theory, this can save you a lot of space. But I haven't used this in practice.

## Less Useful `ni`-specific Perl Extensions


### Array Functions
  * `clip`
  * `within`
  

   
## Writing Your Own `ni` Extensions
**TODO** Understand how this works

## Obscure Interops/Extensions

* [SQL](sql.md)
* [PySpark](pyspark.md)
* [Scripting](script.md)
* [HTTP Operations](net.md)
* [Defining `ni` functions](fn.md)

## More `ni` modules

* Image
* Binary
* Bytestream
* gnuplot


# Lambda abbreviations
## Hadoop example (first-order lambdas without closure variables)
ni uses a lot of higher-order operators (i.e. their arguments are lambdas), and
provides some syntax to make this more compact than specifying everything in
full form. `-H`, or `--hadoop`, is a good example of this as it takes three
lambdas:

```sh
# long form: fully specified lambdas using bracket notation
$ ni hdfs:/data -H [ -t/foo -1f ] [ ] [ -A [ -1st+1 ] ] > output

# short form: compressed as described below
$ ni hdfs:/data -H[t/foo 1f]:^A1st+1 > output
```

Note that although it's nonsensical to have a nested hadoop job, ni doesn't
recognize this subtlety when parsing coerced lambdas. As a result, you can't do
this to chain hadoop jobs:

```sh
# broken; this will try to hadoop further from inside the reducer
$ ni hdfs:/data -H[t/foo 1f]:^A1st+1H... > output

# this is what you want, despite the two extra characters
$ ni hdfs:/data -H[t/foo 1f]:^A1st+1 -H... > output
$ ni hdfs:/data -H[t/foo 1f]:[A1st+1]H... > output      # or this; also works
```

### Condensing the first lambda
The first lambda does two things: take lines matching `/foo/`, and then select
the first TSV field.

```sh
# bracket space elision
$ ni hdfs:/data -H[t/foo 1f][][A [ -1st+1 ]] > output
```

That's about the best we can do here since the mapper requires two separate
commands. If it were a single command we could use coercion from `-H` to do
this:

```sh
# long form
$ ni hdfs:/data -H [ -gc1f ]// > output

# coerced short form
$ ni hdfs:/data -Hgc1f// > output
```

### Condensing the second lambda
```sh
# identity lambda condensed into : and packed onto the close bracket
# (specifies that we want a combiner, but it's the identity)
$ ni hdfs:/data -H [ -t/foo -1f ]:[ -A [ 1st+1 ] ] > output

# null lambda condensed into / and packed onto the close bracket
# (since we don't use a combiner)
$ ni hdfs:/data -H [ -t/foo -1f ]/[ -A [ -1st+1 ] ] > output
```

### Condensing the third lambda
```sh
# lambda coercion (based on the operator's defined syntax)
$ ni hdfs:/data -H [ -t/foo -1f ] : [ -A1st+1 ]

# bracket space elision
$ ni hdfs:/data -H [ -t/foo -1f ] : [A1st+1]

# ^ quoting abbreviation
$ ni hdfs:/data -H [ -t/foo -1f ] : ^A1st+1
```

## First-order lambdas with closure state
Let's suppose you want to compute approximate quantiles of a large dataset.
Quantiles require pre-aggregation, which produces fairly small values that can
be interpolated into a lambda using shell substitution:

```sh
$ ni hdfs:/data -H ^r'qs = [$(ni hdfs:/data/part-00000 -o#100er)]
                      qs.find_index {|q| ai <= q}' //
```

Shell substitution, of course, is a verbose way to do this because you're
unnecessarily typing `$(ni` and `)`, and because you're reinitializing `qs`
each time around.

```sh
# option 1: read the lambda (not any shorter in character terms, and repeats
# work since the initialization is distributed)
$ ni hdfs:/data -H ^r'qs = cache {rl "hdfs:/data/part-00000 -ot#100er"}
                      qs.find_index {|q| ai <= q}' //

# option 2: stream-aliasing fork to create a variable (fast and concise)
# technically a little different; we're taking 10K records instead of reading
# just the first partfile. But the same idea.
$ ni hdfs:/data @qs^t1E4ot#100 -H ^r'qs.find_index {|q| ai <= q}' //
```

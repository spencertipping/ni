# Row operations
These are fairly well-optimized operations that operate on rows as units, which
basically means that ni can just scan for newlines and doesn't have to parse
anything else. They include:

- Take first/last N
- Take uniform-random or periodic sample
- Rows matching regex
- Rows containing specified fields
- Rows satisfying code
- Reorder rows
- Count identical rows
- Joining sorted rows

## First/last
Shorthands for UNIX `head` and `tail`.

```bash
$ ni n10r3                      # take first 3
1
2
3
$ ni n10r~3                     # take last 3
8
9
10
$ ni n10r-7                     # drop first 7
8
9
10
```

Using commands like `r10` can break pipes and cause you to get less data than
you might expect; see "pipe signals" in [warnings.md](warnings.md) (`ni
//help/warnings`) for details.

A "safe" version of `ni`'s "take first N" command exists, called `rs`. `rsN` will have the same result as `rN`, but `rs` will be much slower and should only be used when a pipe signal must be avoided, for example during a [Hadoop Streaming](hadoop.md) job.

```bash
$ ni n10 rs3
1
2
3
```

## Sampling
```bash
$ ni n10000rx4000               # take the 1st of every 4000 rows
1
4001
8001
$ ni n10000r.0002               # sample uniformly, P(row) = 0.0002
1
6823
8921
9509
```

It's worth noting that uniform sampling, though random, is also deterministic;
by default ni seeds the RNG with 42 every time (though you can change this by
exporting `NI_SEED`). ni also uses an optimized Poisson process to sample rows,
which minimizes calls to `rand()`.

**NOTE:** If you're sampling from a large dataset, you will often get some
speedup by parallelizing the row operator. For example:

```sh
$ ni /path/to/large/data rx100          # ~300MB/s
$ ni /path/to/large/data S8rx100        # ~800MB/s
```

See [scale.md](scale.md) for details about horizontal scaling.

## Row matching

There are several ways you can keep only rows matching a certain criterion.

The simplest way is to use a regex:
```bash
$ ni n10000r/[42]000$/
2000
4000
$ ni n1000r/[^1]$/r3
2
3
4
```

These regexes are evaluated by Perl, which is likely to be faster than `grep`
for nontrivial patterns.

You can also row match using code in any language that ni supports. At current,
it supports Perl, Ruby, and Common Lisp.

`rp` means "select rows for which this Perl expression returns true".

```bash
$ ni n10000rp'$_ % 100 == 42' r3
42
142
242
```

The expression has access to column accessors and everything else described in
[perl.md](perl.md) (`ni //help/perl`).

Note that whitespace is always required after quoted code.

**TODO:** other languages

## Column assertion
In real-world data pipelines it's common to have cases where you have missing
data. To remove those, you can select only rows which provide a nonempty value
for one or more columns:

```bash
$ ni n1000p'r/(.)(.*)/' r15     # until 10, the second field is empty
1	
2	
3	
4	
5	
6	
7	
8	
9	
1	0
1	1
1	2
1	3
1	4
1	5
$ ni n1000p'r/(.)(.*)/' rB r8   # rB = "rows for which field B exists"
1	0
1	1
1	2
1	3
1	4
1	5
1	6
1	7
```

```bash
$ ni n10rB | wc -l              # no field B here, so no output
0
```

```bash
$ ni n10p'r a; ""' rA | wc -l   # remove blank lines
10
```

### Set membership
You can also assert that a column's value is one of a set of things. If you
want to do this for a very large set, you should consider using [bloom
filters](bloom.md).

```bash
$ ni n100 riA[n4 p'a*2']        # intersect column A with values from n4 p'a*2'
2
4
6
8
```

An uppercase `I` does the opposite, rejecting specified values:

```bash
$ ni n10 rIAn5
6
7
8
9
10
```

## Sorting
ni has four operators that shell out to the UNIX sort command. Two are
alpha-sorts:

```bash
$ ni n100n10gr4                 # g = 'group'
1
1
10
10
$ ni n100n100gur4               # u = 'uniq'
1
10
100
11
```

The idea behind `g` as `group` is that this is what you do prior to an
aggregation; i.e. to group related rows together so you can stream into a
reducer.

ni also has two `order` operators that sort numerically:

```bash
$ ni n100or3                    # o = 'order': sort numeric ascending
1
2
3
$ ni n100Or3                    # O = 'reverse order'
100
99
98
```

### Specifying sort columns
When used without options, the sort operators sort by a whole row; but you can
append one or more column specifications to change this. I'll generate some
multicolumn data to demonstrate this (see [perl.md](perl.md) (`ni //help/perl`)
for an explanation of the `p` operator).

```bash
$ ni n100p'r a, sin(a), log(a)' > data          # generate multicolumn data
$ ni data r4
1	0.841470984807897	0
2	0.909297426825682	0.693147180559945
3	0.141120008059867	1.09861228866811
4	-0.756802495307928	1.38629436111989
```

Now we can sort by the second column, which ni refers to as `B` (in general, ni
uses spreadsheet notation: columns are letters, rows are numbers):

```bash
$ ni data oBr4
11	-0.999990206550703	2.39789527279837
55	-0.99975517335862	4.00733318523247
99	-0.999206834186354	4.59511985013459
80	-0.993888653923375	4.38202663467388
```

Columns can be suffixed with `g`, `n`, and/or `-` modifiers to modify how they
are sorted (these behave as described for `sort`'s `-k` option), and ni prefers
this interpretation:

```bash
$ ni data oBg r4                # 'g' is a modifier of B, not another sort
11	-0.999990206550703	2.39789527279837
55	-0.99975517335862	4.00733318523247
99	-0.999206834186354	4.59511985013459
80	-0.993888653923375	4.38202663467388
$ ni data oB g r4               # 'g' is a sorting operator
1	0.841470984807897	0
10	-0.54402111088937	2.30258509299405
100	-0.506365641109759	4.60517018598809
11	-0.999990206550703	2.39789527279837
```

### Grouped sort
The `gg` operator sorts key-grouped bundles of rows; this is a piecewise sort.
For example, to sort words within lengths:

```bash
$ ni i{foo,bar,bif,baz,quux,uber,bake} p'r length, a' ggAB
3	bar
3	baz
3	bif
3	foo
4	bake
4	quux
4	uber
```

The first column specifies the grouping key, and subsequent columns are
interpreted as for the `g` operator.

```bash
$ ni i{foo,bar,bif,baz,quux,uber,bake} p'r length, a' ggAB-
3	foo
3	bif
3	baz
3	bar
4	uber
4	quux
4	bake
$ ni n10p'r "a", a' ggABn- r4
a	10
a	9
a	8
a	7
```


## Counting
ni gives you the `c` operator to count runs of identical rows (just
like `uniq -c`).

```bash
$ ni //license FWpF_ > word-list
$ ni word-list cr10             # unsorted count
1	ni
1	https
1	github
1	com
1	spencertipping
1	ni
1	Copyright
1	c
1	2016
1	Spencer
$ ni word-list gcr10            # sort first to group words
1	2016
1	A
1	ACTION
1	AN
1	AND
2	ANY
1	ARISING
1	AS
1	AUTHORS
1	BE
$ ni word-list gcOr10           # by descending count
7	to
7	the
7	OR
6	THE
5	Software
4	of
4	and
4	OF
4	IN
3	SOFTWARE
```

## Joining
You can use the `j` operator to inner-join two streams on the first column
value. ni assumes both streams are sorted already, e.g. using the `g` operator.

```bash
$ ni word-list p'r a, length a' > word-lengths
$ ni word-list gj[word-lengths g] r10
2016	2016	4
A	A	1
ACTION	ACTION	6
AN	AN	2
AND	AND	3
ANY	ANY	3
ANY	ANY	3
ARISING	ARISING	7
AS	AS	2
AUTHORS	AUTHORS	7
```

As shown above, joins are strictly line-sequential: every output consumes a
line from each stream. This is unlike the UNIX `join` command, which outputs a
Cartesian product.

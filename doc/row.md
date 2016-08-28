# Row operations
These are fairly well-optimized operations that operate on rows as units, which
basically means that ni can just scan for newlines and doesn't have to parse
anything else. They include:

- Take first/last N
- Take uniform-random or periodic sample
- Rows matching regex
- Rows satisfying code
- Reorder rows

## First/last
Shorthands for UNIX `head` and `tail`.

```bash
$ ni n:10r3                     # take first 3
1
2
3
$ ni n:10r+3                    # take last 3
8
9
10
$ ni n:10r-7                    # drop first 7
8
9
10
```

## Sampling
```bash
$ ni n:10000rx4000              # take every 4000th row
4000
8000
$ ni n:10000r.0002              # sample uniformly, P(row) = 0.0002
1
6823
8921
9509
```

It's worth noting that uniform sampling, though random, is also deterministic;
by default ni seeds the RNG with 42 every time (though you can change this by
exporting `NI_SEED`). ni also uses an optimized Poisson process to sample rows,
which minimizes calls to `rand()`.

## Regex matching
```bash
$ ni n:10000r/[42]000$/
2000
4000
$ ni n:1000r/[^1]$/r3
2
3
4
```

These regexes are evaluated by Perl, which is likely to be faster than `grep`
for nontrivial patterns.

## Code
`rp` means "select rows for which this Perl expression returns true".

```bash
$ ni n:10000rp'$_ % 100 == 42' r3
42
142
242
```

The expression has access to column accessors and everything else described in
[perl.md](perl.md) (`ni //help/perl`).

**TODO:** other languages

## Sorting


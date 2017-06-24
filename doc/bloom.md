# Bloom filters
ni implements a minimalistic bloom filter library to do efficient membership
queries. This is visible in two ways -- first as a set of Perl functions:

- `$f = bloom_new($n, $p)`: creates an empty bloom filter designed to contain
  `$n` elements with a false-positive probability of `$p`.
- `bloom_add($f, $x)`: adds the string `$x` to `$f` destructively.
- `bloom_contains($f, $x)`: returns 1 if `$f` might contain `$x`, 0 if it
  definitely does not contain `$x`.

These functions are available inside all Perl contexts. The other, and more
common, way that you'd use bloom filters is by a trio of operators `zB`
(compress into bloom filter), `rb` (rows which match bloom filter), and `rB`
(rows which don't match bloom filter). The syntax is this:

```
$ ni rows.txt zB45 \>filter     # compress lines as elements
                                # resulting binary filter will store 10^4 items
                                # at 10^-5 false positive rate

$ ni other-data.txt rbAfilter   # return rows of other-data.txt for which field
                                # A occurs inside the bloom filter

$ ni other-data.txt rBAfilter   # return rows of other-data.txt for which field
                                # A does not occur inside the bloom filter
```

Because the filter data is just a lambda, it's also possible to say something
like `rbA[rows.txt zB45]`. For example:

```bash
$ ni nE4 rbA[i108 i571 i3491 zB45]
108
571
3491
$ ni nE4 rBA[nE4 rp'a != 61 && a != 108' zB45]
61
108
```

You can also stuff a bloom filter into a data closure:

```bash
$ ni ::bloom[i108 i571 i3491 zB45] nE4 rbA//:bloom
108
571
3491
```

# Spreadsheet metaphor
ni is a spreadsheet in the conceptual sense but not in the concrete sense. To
mask its identity-insecurity a little, it supports an only slightly misleading
spreadsheet-style notation for cell addressing. For example:

```sh
# average word length, without using the -A shorthand
$ ni /usr/share/dict/words -p'r length A' -snp'r B / A'
```

You can also get finite-distance backreferences:

```sh
$ ni /usr/share/dict/words -p'r A0 ^ A1'
```

## The `r` function
Each of ni's operators takes a spreadsheet and returns a new one. Normally if
you use some code that returns an expression, that expression's value will be
added as a new column:

```sh
$ ni /usr/share/dict/words -p 'length A'
A       1
A's     3
AA's    4
...
```

However, you can replace the row's contents by using `r`:

```sh
$ ni /usr/share/dict/words -p 'r length A'
1
3
4
...
```

You can also replace one or more cells' contents using `c`:

```sh
$ ni /usr/share/dict/words -p 'c A => length(A), B => A'
1       A
3       A's
4       AA's
...
```

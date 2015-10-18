# Spreadsheet abstraction
ni mapping code, e.g. from `ni -m'...'`, is executed within the context of a
streaming spreadsheet. This is a much more powerful paradigm than nfu's
single-row operations, as shown below.

## Single-row operation
```sh
$ nfu data -k '%2 > 100' -m 'row %0, %1 * %2'
$ ni data -m'r a0, b0 + c0 if c0 > 100'         # with row offsets
$ ni data -m'r a, b + c if c > 100'             # shorter implicits
```

You can coerce the cell type using a suffix:

- `a0` or `a`: default type (usually a string)
- `a0s` or `as`: coerce to string
- `a0i` or `ai`: coerce to integer
- `a0d` or `ad`: coerce to double or float
- `a0e` or `ae`: coerce to boolean (does the cell contain a value)

## Buffered lookahead
```sh
$ nfu data -S                                   # builtin delta operator
$ ni data -S
$ ni data -m'r a1d - a0d'                       # same, but with lookahead
```

By default, lookahead doesn't consume extra rows -- that is, the next iteration
will use the buffered row rather than skipping it -- and all lookahead
references are immediately-resolved concrete values. If you look ahead beyond
the end of the stream, those cells will evaluate to `nil`, `undef`, or similar.

If you want to skip over the lookahead region or a subset of it (i.e. consume
rows), you can do this in two ways. One is to suffix a lookahead cell with `!`,
e.g. `a1d!`, which indicates that the next iteration will start at row 2, not
row 1. The other is to invoke `seek! 2`, which has the same effect.

## Buffered ranges
Buffered ranges have three notations with different meanings:

- `b5_8`: `[b5, b6, b7, b8]` (partial column vector)
- `b_g3`: `[b3, c3, d3, e3, f3, g3]` (partial row vector)
- `b3_d4` or `b3d4`: `[[b3, b4], [c3, c4], [d3, d4]]` (matrix)

Sometimes you want to select ranges based on cell values, which you can do
using relative conditioning:

- `b5_Ea` or `b5Ea`: `[b5, b6, ..., bK]` for the largest K such that `a5 = a6 = ... = aK`
- `b5_LEa` or `b5LEa`: `[b5, b6, ..., bK]` ... `a5 <= a6 <= ... <= aK`
- `b5_dEa` or `b5dEa`: `[[b5, c5, d5], ..., [bK, cK, dK]]`

Selection operators are:

- `E` or `EQ`: equality of cells
- `NE`: nonequality of adjacent cells
- `G` or `GT`: ascending
- `L` or `LT`: descending
- `LE`: nonascending
- `GE`: nondescending
- `S`: equal sign (positive/zero or negative)
- `Z`: all zero or all non-zero
- `N`: take N cells, where N is the number in the beginning cell

Selected ranges can be suffixed with `!` to specify that all rows loaded are
consumed prior to starting the next iteration.

## Lazy ranges
Lazy ranges work just like buffered ranges, with the following exceptions:

1. Column names are specified in uppercase, selection operators in lowercase.
2. Lazy ranges are always completely consumed since no buffering occurs.
3. Lazy ranges are abstract values and must be reduced to get a value out.

You can construct a lazy range that overlaps a buffered range with no change in
behavior.

## Addressing
Every operator has an implicit "address", or set of columns upon which it
operates. The default address of every builtin operator is either all columns
(for operators that make sense against multiple fields, like `-m`), or just the
first column (for single-value operators like `-g`). You can readdress an
operator by prefixing it with an address spec, which takes one of these two
forms:

- `\d+`, e.g. `-041mr'foo'`: address just the columns at the specified base-10
  indexes
- `[0-9a-zA-Z]+@`, e.g. `-bc@mr'foo'`: indexes in base-62 (0-9, a-z, A-Z)

Addresses can duplicate and reorder columns. Unaddressed columns pass through
unmodified. For example:

```sh
$ ni /usr/share/dict/words -mr'row %0, l.size' > wsizes
$ ni wsizes -1mr'%0 * %0'              # word, size²
$ ni wsizes -11mr'%0 + %1'             # word, size + size
```

Operators like `-a` can collapse multiple lines into fewer outputs. In this
case the unaddressed fields are treated as columns and truncated to fit the
number of output rows. If there are more outputs than inputs, extra output rows
will have blank values.

If an operator emits fewer columns than its address, the unspecified columns
are filled with blank values. If it emits more, extra columns are tacked onto
the end of the input row (after assembling all addressed column values).

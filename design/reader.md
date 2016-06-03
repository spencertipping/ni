# Reader interface
Whether or not you're processing data that lends itself to a spreadsheet
abstraction, you're ultimately addressing a sequential stream using some kind
of reader. The goal is to make the reader as useful as possible while
preserving the declarative advantages of array notation.

## Typical stream processing workflow
A FIFO mapper will most likely work like this:

1. Read forwards until we have a "record of data", whatever that means.
2. Optionally read more records, perhaps conditionally.
3. Return or side-effectfully emit output records.

Laziness interleaves the process:

1. Read forwards until we have more records.
2. As we read records, update any active lazy computations.
3. As lazy computations complete, emit results.
4. Return lazy values that will eventually become output records.

## Lazy by default
We need explicit modeling for lazy values; we can't get away with any shortcuts
like fibers or continuations. Here's why:

```sh
$ ni m'r (a../foo/).sum.to_f / (a../foo/).size'
```

The division operator needs to evaluate both operands, which means nothing in
either one can suspend execution. So the reducer abstraction probably makes
sense.

**By default, lazy computations consume rows -- so the mapper won't be invoked
again until all lazy computations are complete.** This sidesteps the whole
question of interleaved stuff, which is counterintuitive and treacherous.

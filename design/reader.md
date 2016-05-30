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

# Destructuring
Most data extraction workflows don't use every key of a rich data object like
JSON or XML. ni allows you to avoid the overhead of fully decoding these
objects by using targeted extraction, which compiles an optimized function to
return just the values you need. Depending on what you're extracting and which
optimizations you allow ni to use, this can be up to 20-30x faster than doing a
full decode.

## Decoding JSON
The JSON decoder supports the following operations:

- `:foo`: match every value of a "foo" key (descend)
- `.foo`: match the first value of a "foo" key (don't descend)
- `[foo,bar]`: match the values of the "foo" and "bar" keys in the same object
  (don't descend)

Operators can be combined in two ways:

- `op1,op2`: value(s) from op1 followed by value(s) from op2
- `op1op2`: match op2 within the value specified by op1

**TODO:** Structured row delimiters instead of the default `\n`

### Schema inference
ni looks at a bunch of records up front to determine which optimizations are
possible. Specifically, it looks for a few invariant conditions:

1. The set of keys is bounded (i.e. no fully-variant keys)
2. Within objects, the set of keys is consistently ordered
3. A key is consistently present at a given path
4. A key is uniquely present at a given path
5. The pattern of whitespace (if any) around a key is consistent
6. The structure of the value referred to by each path

If your JSON is _unstable_ -- that is, these invariants appear to be present
but then change -- then the destructuring output may be incorrect. You can
prevent this by doing something **TBD**.

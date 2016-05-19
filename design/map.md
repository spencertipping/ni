# Map function support
Not all text formats lend themselves to TSV mapping; for instance, XML is a
disaster if you want to parse it this way. It seems like we want multiple
accessors, each with its own way of consuming the stream. This should also
make it possible to handle binary formats.

Binary formats are rarely straightforward; there's likely to be a lot of
arbitrary, sometimes conditional, seeking (e.g. for jump instructions within
an x86 instruction parser, or when parsing a file system image). Maybe we want
some kind of queue-map thing that lets you specify the format and
continuation(s).

**Q:** Given that we're doing continuation maps, how do we unify results from
the substreams? (Section below)

So the text-spreadsheet abstraction is most likely just one of many, though
perhaps it's the most well-developed mode since the input space is also the
most predictable.

## Binary/text type erasure
We can't use the same `m` operator across protocols because the promise is
that we have another "record" of data in the readahead queue each time the `m`
code is called. We'd know this only given knowledge of the protocol (while I
suppose there's an argument to be made that bytes constitute a record of some
sort, I'm not convinced it's a good one).

So **the operator dictates the decoder and record queueing logic**. Ideally
the operators would be very simple, but I'm not completely sure how to do it
yet -- particularly for dispatch-heavy binary formats. I don't want a
situation where binary and text data have poor interoperability.

## Binary and stream combination
Let's assume we're parsing binary with the sequential-input constraint; that
is, the input isn't seekable. Then continuations are stored in a queue to be
consumed as we see more input. So if we had something like this:

```sh
$ ni bCl 'a == 0xe8 ? goto b : ...'
```

The `goto` in this context must refer forwards (which is obviously misguided
for executable files, though perhaps we can configure a lookbehind window).

Substreams illustrate the other problem with this model. Suppose we dispatch
by parse state, e.g. for a RIFF wave file, maybe we parse the header
differently from the rows. Then we have this:

```sh
$ ni Bhead {head   11L    'st_16bit_le(_) ? _2x16le[0] : uh oh' \
            2x16le 's<s<' 'r a'} \
     ...
```

## Binary parsing libraries
It's useful to have a series of compact definitions suitable for parsing
binary data, possibly with pluggable lambdas -- particularly given that
performance drops quickly as soon as IPC is involved for the decoded data
(i.e. binary is the most efficient transport form, so let's contain the
decoded representation to a single address space).

### Library programmability vs decoders
The use case depends on the format we're dealing with. For example, WAV files
are linear and simple to decode into a stream of values; so are gzip files,
etc. These are mostly-generic data container formats.

x86 executables, on the other hand, are complex and it isn't clear what you
would do with them. Emit basic blocks, maybe? Then there's the linking
problem. Maybe this is a bad example for what you'd use ni to do, though it
seems like ni has an application as a generic format transcoder.

> Actually, that's just it: as a transcoder, ni would by default be
> structure-preserving -- even when that structure isn't especially helpful
> for data mining. So something to reduce x86 instruction streams to anything
> that's easier to work with, maybe a spreadsheet of address + instruction +
> operands, would be sufficiently useful to justify a standard library for it.

### Thrift: a case where customization is required
Thrift binary streams (I assume; todo verify) are almost entirely untyped and
use a universal varint-tagging protocol to mark fields -- though required
fields might be different. Either way, I'm going to assume that it's
impossible to decode a binary thrift object without having the schema
available.

So when you go to decode some thrift binary thing, you'll need to point ni to
the .thrift file(s) and indicate which of the structs you're starting with.
Maybe the simplest strategy is to use the thrift compiler itself to generate
the appropriate Ruby/Perl libraries; then we'd have a thin wrapper library to
make access more concise, and we'd probably modify the thrift objects to
support a shared retrieval-path syntax.

Thrift also demonstrates a case where the ideal interop extends far into the
language itself; thrift objects don't seem inherently linearizable.

## Buffering and scope of reference
Line processing is straightforward given a the limited frame of reference
that's typical of most text files: each line can be processed independently in
most cases, but within that line we assume arbitrary data dependence (e.g. a
JSON object per line). So the GC aspect is very well-defined: process a line,
forget it, and move on.

Any sort of column-wise transformation breaks this paradigm: now we're
potentially committed to an arbitrarily large seek range. Map/reduce helps
somewhat by assuming a power-law distribution (?) of true data locality, then
isolating the heavy lifting to dedicated partition+sort operations.

I suppose ni should use the same design pattern, though sorts are a bummer
because they block (design/group.md). If the access pattern is encoded into
the data itself or the underlying data is seekable, then we may have better
options. A good example is when we want to do a random-access join against a
sorted file, rather than buffering+sorting the left side.

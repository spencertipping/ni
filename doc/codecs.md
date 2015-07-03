# Codecs
Codecs complicate things. How important are they, really?

Obviously they're tremendously useful; we want general-purpose ways to read
binary data, often streaming, and codecs do this. But codecs-as-programs are
way too complicated. It makes a lot more sense to have some kind of
step-by-step conversion process to decode binary data.

One of the challenges of reading binary data is that it contains offsets and
other stuff that requires logic to parse. It's fundamentally more complex than
text data, or at least has the potential to be. (And to be fair, we can't
reliably parse text data with a streaming model; not in the general case
anyway.)

Binary data is challenging for two reasons:

1. Read order is often loosely defined; we need to be able to seek.
2. Data types are rarely self-evident. Seeking establishes context.

(1) implies the existence of typed seek queues, some type of bookmarking and
offset mechanism, and polymorphism and repetition. All of this involves
maintaining a dialog with ni, which in turn implies some type of programming
environment.

If the goal is to maintain transparency, then we can have a codec be a program
that emits two things: a metadata field to dictate what to do next, and an
output field to encode stream production. In this case a few things are true:

1. ni maintains a protocol specifically for such metadata.
2. "default" formats like TSV still use metadata, possibly to seek beyond
   delimited values.
3. To the extent that we're executing a program, the program blocks to emit
   output. Internally we'll need some type of continuation logic (though just
   linear, so we can model it with blocking execution).

Programs require some type of addressable memory, which unless emitted as
metadata would be hidden state. Maybe model the program explicitly as a state
reduction? (So a seek would include some arbitrary data to be recovered later,
probably by a branch map.)

Having them manage their own state as a reduction means that codecs become
one-way programs; a codec used for encoding can't decode anything and vice
versa. The proof here is that metadata isn't fundamentally recoverable, nor
does it reliably accompany the data; ni consumes it as things are read. The
simplest solution here is to not restrict codecs to require invertible
execution.

State reductions aren't quite right for this. It implies that each record is
generated monomorphically, which is not especially useful for complex formats.
In general, leading context isn't enough to fully specify a record, and
moreover we don't want to impose record-interpretation limitations this way. ∴
we need a more general execution model than a simple reduction.

Let's state some requirements:

1. Every side-effect of a codec must be serializable (e.g. serializable
   metadata that follows a known protocol).
2. Codecs must be able to execute arbitrary logic to decide the interpretation
   of some piece of data, including polymorphism within a record. This implies
   that codecs aren't generally invertible.

Ok, stepping back for a moment: is it possible to separate parsing stages? We
want to avoid monolithic chunks of logic.

The assertion of nfu and ni is that stream processing gives you enough tools to
do what you want to do. Let's assume for a moment that arbitrary stream
decoding can be expressed as a mapping function over a lazy stream of bytes
encoded as a single record. You can use an arbitrary language to manipulate
that stream, and any record you emit may block to avoid space leaks.

We can't quite unify these two models. Even in Canard, where arguably we have
the most control over GC, we still face the problem that lazy streams of fields
won't behave the same as regular streams of fields in a mapping or reduction
context. Codecs seem to be solving a fundamentally different kind of problem.

Is it possible to have a multi-stage pipeline in which later stages emit
metadata instructions for earlier ones? Or better yet, what if we push the
codec programming layer into the regular pipeline?

We have another interesting issue. Suppose there's a general-purpose operator
to decode a stream using a codec of some sort. How would this operator behave
on data without an intrinsic byte representation? (i.e. something already
decoded) It seems like codecs are some type of quasifile-related abstraction
and not something that applies in the general pipeline. This makes especially
good sense in the context of reinterpretation, which is a memory buffer with a
write-encoder and read-decoder that are different.

Can we name a stream and then send commands to it from later stages? Arguably a
block/unblock cycle is a command of a sort, but in a particularly controlled
way. I'm not sure we want to be sending arbitrary commands backwards. It's
better to have some type of lambda-based feedback loop that traverses the file
and uses stream aliasing to emit values.

## Stepping back for a moment
Codecs seem wrong. We almost never use them to transcode something, and all
records/fields should be displayed as TSV just as they are in nfu. In the rare
case where we want to consume or generate nonstandard data, there should be
adapters to help with that. We might not ever really need low-level stuff built
into ni. Let's consider codecs a non-issue except for IPC.

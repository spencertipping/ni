# Map function support
Not all text formats lend themselves to TSV mapping; for instance, XML is a
disaster if you want to parse it this way. It seems like we want multiple
accessors, each with its own way of consuming the stream. This should also
make it possible to handle binary formats.

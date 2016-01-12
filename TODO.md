# Things to do/fix
- ni's internal filesystem should use an inode table and support symlinks +
  binary data, the latter of which should have been obvious because its
  internal format is binary.
- Don't write parser combinators in POSIX shell.
- Consider making ni a jit-to-C compiler for the whole pipeline, sort of like
  the original design.
- Design in selective op-fusion for things like `-st+1`; there's no excuse to
  have two processes, one of whose output is almost entirely discarded.

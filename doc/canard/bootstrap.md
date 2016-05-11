# How canard is bootstrapped
ni takes a pragmatic approach to bootstrapping canard, at least for the moment.
A lot of standard library functions are written in Perl, both for performance
and expediency. I'm not making any particular attempt at metacircular
minimalism here because ni doesn't do much compiler-metaprogramming yet (though
as canard progresses independently I'll probably start pulling that bootstrap
logic into ni).

ni's bootstrap components consist of:

- The core interpreter loop
- Some natives for μ-canard primitives (cons, uncons, etc)
- Some convenience functions for writing resolver entries
- Some list manipulation functions like `://` (reconsing substitution) and `+`
  (append)

This is implemented in [src/canard.pl.sdoc](../../src/canard.pl.sdoc).

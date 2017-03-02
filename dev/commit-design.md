# Commit design log
## Replayability
If the implication is that commits give us a way to encode and reproduce global
state, we need to think carefully about where the limits are. Does this include
our FD table? Does it include external processes and the state of data through
them? (In an ideal world, sure -- for robustness -- but maybe data is sent
through "blind commits" that can't be replayed.)

Actually let's think this through for a minute. Commits don't need to contain
the data to be useful; instead, the data file is assumed to be
arbitrary/mutable, and commits represent the _operations_ applied to that data.
So pipelines are made of commits; certainly true because they impact the real
world.

This is actually quite useful, because an unapplied commit can still be
reasoned about. We can consider alternatives and apply the commit to the world
when we have one that is likely to work.

## Commits as changes to perl state
Perl performance is invariant wrt the length of an inheritance chain, so
commits can create perl packages whose inheritance reflects commit
dependencies. They can also destroy packages as they are garbage-collected,
though some perl versions will leak memory if we rely on this.

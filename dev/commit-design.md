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
### NOPE (original design, now superseded)
Perl performance is invariant wrt the length of an inheritance chain, so
commits can create perl packages whose inheritance reflects commit
dependencies. They can also destroy packages as they are garbage-collected,
though some perl versions will leak memory if we rely on this.

## Commits as changes to perl state
Perl's symbol table is the mutable thing we need to protect, so let's use
commits to journal the changes we make. Things we know about commits:

1. They're immutable (so behaviors aren't all commits, though slices can be).
2. They're invertible.
3. Inverting all of them would undefine ni from a perl instance, modulo module
   loading.
4. They can be serialized, which is a big can of worms.

### Reckless promises like serializing commits
(4) is a big deal because commit serialization isn't easy: how do we serialize
functions with closure state or something? If one commit refers to data
serialized inside another commit, we ultimately need the references to be
shared in order for the state to be properly replicated. This implies that
we're pushing object identity into the symbol table somehow, or at least
maintaining weak references in some kind of "commit interpreter" (neither of
which is a great solution).

Part of the philosophical problem here is that Perl's objects are effectively
anonymous, yet their identity matters: `[] != []` in a mutable context, and
mutable contexts obviously will exist. Options to resolve this:

#### Deanonymize all objects at serialization-time
This is what the perl image codegen does, and it losslessly restores object
identity as we expect. This works if the serialization entry point is
monolithic, but we can't use it to distribute identity resolution because perl
uses unpredictable memory addresses for things (which means the resulting
commits would be equally unstable, despite ni being in the same state).

#### Objects own references; identity doesn't matter
This might actually work. If objects never share references with each other,
then we don't need to care about identity; each object can generate a commit
that reproduces its state _structurally_. There's a lot to recommend this
separation of concern, though it's unclear whether we can fail early if someone
writes code that doesn't work this way (and some classes might hold anonymous
references to other instances, like for futures -- which arguably can't be
serialized anyway so maybe it doesn't matter).

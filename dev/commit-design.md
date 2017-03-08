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

**Revision:** (4) is impossible. Commits cannot be individually serialized
because object identity is contextual. We can journal modifications to Perl
state, but that journal inherits the context that generated our serialization.
Commits are not independent objects.

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

Is it possible for objects to obtain stable canonical IDs? Nope, proven below.

##### Commits and structural identity
The idea behind a commit ID is that it stably encodes a ni runtime state: if
you run through the same process to get to a given state, then the commit ID
should be predictable. This means we can't use things like memory addresses to
get there, or at least if we do we have to normalize them by (some process that
is more involved than I'm inclined to implement).

So ... if we want to be able to look at an object and form a stable commit
around it, we'll need to know who else refers to anything about that object,
etc. In other words, we're taking an unbounded snapshot of the runtime and
there isn't a good way to wall off any aspect of state.

**Proof:** Deanonymized objects, even within an object graph, have no stable
identifiers.

> We can construct two arrays `@a` and `@b` such that `@a = (\@b)` and `@b =
> (\@a)`. The two arrays can't be differentiated from one another structurally,
> but someone could be referring to either one -- in other words, an
> unobservable aspect of their identity is significant. Any IDs we assign them
> absent the external reference information will appear unstable from the outer
> reference frame. Therefore, no stable deanonymization exists for
> partially-serialized images.

Put differently, _object identity is contextual._

#### Objects own references; identity doesn't matter
This might actually work. If objects never share references with each other,
then we don't need to care about identity; each object can generate a commit
that reproduces its state _structurally_. There's a lot to recommend this
separation of concern, though it's unclear whether we can fail early if someone
writes code that doesn't work this way (and some classes might hold anonymous
references to other instances, like for futures -- which arguably can't be
serialized anyway so maybe it doesn't matter).

This is actually a more onerous approach than I'm willing to entertain.
Behaviors hold references to other behaviors, for example, and slices know
which packages they've been applied to (all mutably). As ni evolves it's highly
likely that we'll want shared anonymous state between objects. [is it?]

Ok, this gets back to metaclass-oriented serialization. Objects can produce a
packed commit that encodes their state and dependencies -- then objects can
remain anonymous because the context establishing their identity can be assumed
to be quantifiable -- or at least bounded. If they depend on other objects,
these dependencies appear as named references in the commit. If there's
anonymous state, it's owned by a single object.

##### Objects aren't always commits; e.g. slices
A slice can be applied to multiple perl packages, which probably means that if
we reconstruct it it will do the equivalent of producing multiple perl-package
commits. This probably indicates that slices serialize into multi-commit
bundles: one to create the slice, others to install it into each package.

Maybe the simplest strategy is to make the rule that objects side-effect by
journaling rather than by acting. Then we end up with the expedient property
that all modifications to the perl runtime are quoted, which in turn means that
_interfacing with local and remote perl interpreters is equivalent._

##### Constructors can create named objects, but don't return commits
This is relevant in an RMI context. Who owns the commit that is created when a
class comes into existence? Arguably the class itself does; that's how it would
be serialized.

Ok hang on; commits aren't quite as well-defined as I want them to be. A commit
is a way to modify the perl interpreter state, or more specifically to create
symbol table entries. There's no commit for object instantiation; that's
considered to be an internal state modification for an image. Images and their
associated instances are opaque; modifications they make to the perl runtime
are not.

**Q:** Do we really need something as sophisticated as commits? When, beyond
unit testing, do we have a situation where we want some kind of
partially-realized definition set? Why not just have slices un-apply themselves
when they're garbage collected? Or how about `fork`ing the process before doing
destructive stuff like unit testing?

Until there's a compelling use case, I'm going to shelve the idea that we need
this kind of state management.

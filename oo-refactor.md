# Refactor notes
## o47
Right now we've got a heavy metaclass system whose instances serialize
themselves as data structures (which side-effect into defining the Perl
namespace methods required to make it all work). It's not the worst system, but
it's not exactly what we need either. It sends too much state to remotes whose
only purpose would be RMI.

It's simpler, faster, and better to use a different strategy. Metaprogramming
is fine for a bootstrap layer, but the goal shouldn't be to build up in-memory
object state. Instead, we need to generate pieces of code that can be combined
with `pack()` into meta-erased remotes. No need to send things like
documentation, which means we need a way to track piece/piece dependencies. If
we do this right, we can also MD5 the pieces up front and end up with a
constructive history model -- so remotes can be updated in place when new
metaprogramming happens, and history is garbage-collected after being applied
but remotes are still aware of their revision level. (Similar to git, but
without storing the commit history.)

Ultimately we need to model changes to Perl's meta-state as constructive
commits; there isn't another way to reliably synchronize remotes that contain
live data.

### Metaprogramming layer
"Metaprogramming" = "generating commits". Some commits store code, others store
data -- implementation vs documentation (and possibly other stuff like data
closures). OO is fine and probably a good choice: the existing metaclass stuff
can be a little less formal but fundamentally behave the same way. The original
ni image can be constructed using a DSL and probably doesn't need to be
repacked in terms of commits. ni isn't self-replicating in a literal sense
because it no longer needs to be.

### Commit layer
Append-only history, single-author -- so everything is a fast-forward. Commit
data is opaque code to be executed, commit IDs are the MD5 of the data +
dependencies. Remotes are at liberty to apply then deallocate, storing only the
commit IDs.

Each vertical aspect of ni gets its own floating commit ID -- like a branch,
but they're all applied at the same time. This means documentation exists on
one branch, tests on another, and each major feature on its own branch. The
aggregated state is a merge of multiple branches.

Design questions:

1. Do we want to encode merges as commits? Probably not; it complicates
remotes when they have different sets of features installed.

2. Can we use commit IDs as a way to track network traffic state? In other
words, if we've got a mechanism for remote updates either way, can we use it to
help the transport layer track delivery? (Answer below: we shouldn't because
it's complicated and high-overhead.)

#### Q1
It's a question of who's storing the remote state. If we've got N remotes, each
with a potentially different set of features installed, then the hub instance
needs to track the commit ID for each one.

Of course, this is probably true either way: the hub instance needs to at least
know about each remote, and in doing so it's going to store the set of branch
commits for that remote (since it might try to push an update at any moment).
So storing stuff as a merge is a good mechanism for branch scaling: now state
checks are O(1) in branches, not O(n).

#### Q2
Q2 is worth considering. If remotes are tracked as sets of commit IDs, then the
transport layer is really a retry-loop around getting them updated to specified
commits. This puts a size limit on each individual commit, since we're
multiplexing egress links -- though that isn't necessarily a bad thing either.

Ok, so we'd have a branch for transport-layer state and remotes would commit
against it ... now arguably we'd want a separate branch per sender. It isn't
clear that this is particularly simple or worthwhile.

Another argument against Q2: packet loss is used for flow control. We don't
necessarily want meta-updates to compete directly with other L4 traffic -- i.e.
we'd want independent prioritization using QoS or similar.

How about transient branches? Just because connection state != instance state
doesn't mean we can't use commits to advance streams. Transience also implies
sender-locality: the branch isn't mentioned in global state announcements.

This has some nice advantages, including the usual stream polymorphism we get
from normal branch processing. Streams could be represented as code, or as data
that gets consed into something (or as data that gets streamed into some kind
of reduction). Remote update is just a code stream that doesn't have a defined
ending; though this notion conflicts with Q1, since there isn't an obvious way
to implement merges on top of multiple L4 streams -- at least, not at the
protocol layer.

A simple answer would be to manage remote branches independently and forget
about merged signature stuff.

Something's wrong with the branch model: suppose there's a merge-point in the
history. Then we've got shared commits, but multiple branch points. So state is
a DAG. But we don't merge things, which means commits would need to have joint
dependencies -- leaving the commit order unspecified. Is that technically
correct?

Sure, it could be. It's also potentially useful: streams are singly-linked
dependent structures -- so a commit can be applied once all of its dependencies
are present.

One possible issue: how do we allow retroactive branching without storing all
historical commit IDs? Might need to mark commits as being linear so the remote
is at liberty to discard them. That's a nice abstraction, actually: commits
have _reference dispositions_ that hint about future references. Some are
fully transient, some linear, and some branched -- perhaps with an expiration
time. (And maybe future commits can update the reference disposition of past
ones, which might be a more productive model to use.)

...so if we have a stream, EOF would be a commit whose reference disposition
was transient. Intermediate commits to the stream would have reference
dispositions that indicated "ping if no update within X time." ACKs carry
stream state; then the sender can reference future packets wrt confirmed
receipt: "it's commit X plus three" -- so the recipient can bit-vector SACK
against a partial stream.

Commits also make it easy to use things like files as transport-layer devices.
We could tell a remote, "read a bunch of commits from this thing; here's the
anchor point, ignore other stuff" and the DAG connector would selectively cons
or ignore. That would just be wire-format to/from a file or other arbitrary
stream.
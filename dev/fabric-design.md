# Fabric design log
## RMI setup
Bootstrap the image using ni's image generator, but then things get murky. How
do we do follow-up communications?

1. Full-serialization RMI: space leak due to expanding visited-set, and
   possibly-unnecessary eval() overhead.
    - Reset visited-set after each method call? (i.e. rely on named objects for
      referential persistence)
    - Perl can eval() some data at 40MB/s or more -- so how much of an issue is
      this really?
    - Persist named objects?
2. Light-serialization RMI: structural-only for method calls, which means we're
   moving functions and can't easily proxy full RMI.

The simplest way to do this is probably to provide remote `ni()` function
access, then immediately return proxy objects.

Probably should add errors/variadic behavior to futures so we can easily cover
the full range of method behavior. Also need to forward `wantarray` in RMIs.

## Security
It's a problem to have insecure RMI links, but it's also a problem to expect an
RMI peer not to send potentially malicious results. It's not the end of the
world to have performance degrade into oblivion or run out of heap space, but
arbitrary code execution can't be initiated by a peer.

I think this means we need an asymmetric encoding for RMI channels: outgoing
can be evaled, incoming needs to be JSON or another safe format.

**737 update:** Outgoing is an arbitrary commit, incoming is a structural
message that indicates how to resolve the future. This addresses security
issues, particularly if we have a null-committer that we use on the incoming
deserializer.

**837 update:** Commits no longer exist. I'm tempted to say something like
outgoing is arbitrary code, incoming is an arbitrary safe-to-eval value -- but
maybe it's more useful to establish multiple connections to the remote, each
with its own properties. This means the remote can initiate communication
without making a trusted method call (e.g. for some type of event listener that
isn't well-represented by a future object).

## Interface
RMI is one method call deep, and happens when you work with a delegated object.
We can address any named object in the remote, and the wall is around code
within object methods. Results are future-converted. Calls are made from an
event loop, which may become unresponsive if we do something expensive.
Failsafe with `SIGALRM`? (Let's jump off that bridge when we get there. The RMI
parent can always remote-kill it.)

## Persistence of remote objects
Remote objects must manage their own deallocation because their owning process
may become unavailable at any point. This involves a few things:

1. A remote object needs to be aware of its referring set, whether it consists
   of child processes or a parent hub that expects to be able to interact with
   it.
2. Upon reconnection to a remote, a parent hub needs to specify the set of
   objects whose existence it's aware of; the proxy will then inform any
   unmentioned live objects that they've lost a parent reference. (Implying
   that remote objects are enumerable.)

**Q:** Why would a remote object simply continue to exist, absent a local
reason to do so? A few reasons: it could be monitoring something, for example,
or it could be a service that replies to connections. So an object should
specify the conditions under which it continues to exist: "I'm reachable and
still doing work," for instance. This goes for local objects also.

Pings+TTL is a reason for an object to exist.

## RMI and unreliable connections
The RMI protocol needs to acknowledge message receipt separately from reply,
and it needs to use message sequencing like TCP to prevent duplication.
Specifically, the sender sends message `n` and awaits an ack for `n` (holding
the message for replay until the ack is received). Two situations from here:

1. Sender receives the ack and discards the message.
2. Connection is dropped. In this case the sender waits for the connection to
   come back and repeats the message unless its TTL is exceeded.

The receiver tracks acks so it can ignore a duplicate message, which will
happen if the connection dies between message receipt and sender receiving the
ack.

## Multi-level routing and QoS
We need a spanning-tree mechanism to figure out how to route packets between
hosts. Ideally the hub isn't driving all RMI (a hard requirement). Materially,
this means we'll have an indirect messaging path: `A -> (B <-> C)` as a nested
request. `B` and `C` are, in `A`'s reference frame, self-managing objects.

## Replication
ni state is factored into persistent (named) and transient (unnamed, or in a
transient namespace). Replicas contain a complete and dynamically updated
snapshot of persistent state: any modifications to the hub image are
automatically propagated inline with RMIs.

...alternatively, maybe objects themselves are referred to remotely: can
class instances apply to perl namespaces across an RMI connection? Not unless
we've got packets-as-commits going on. **Replica maintenance is a reason for
commits to exist:** issue the series of commits that would produce this state.

Two situations we need to handle:

1. A persistent remote, and the hub has been updated with new logic in general.
2. A transient remote, and the hub has been updated with new logic for that
   remote.

Not clear that these are any different from each other. The question is how we
maintain the state beyond protocol-level reliability. A potential problem, of
course, is that commits aren't garbage-collected. Maybe it's a destructive
update process and it's ok to have to re-exec after some point. Presumably we
can emit runtime state into the exec image in that case.

What's the line between "state that needs to be updated" and "local mutable
stuff", formally speaking? Metaclasses superficially seem to blur the
distinction -- but I suspect they'll ultimately simplify it. Is state
propagated eagerly or lazily?

There's an organizational advantage to having commits, and commits are
obviously an addressible collection whose members can be requested remotely.

## Open questions
1. Commits or fast-propagation?
2. Any special-casing around intermittent connections?
3. Are all remotes the same type of object? Are some partial images?
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

## Interface
RMI is one method call deep, and happens when you work with a delegated object.
We can address any named object in the remote, and the wall is around code
within object methods. Results are future-converted. Calls are made from an
event loop, which may become unresponsive if we do something expensive.
Failsafe with `SIGALRM`? (Let's jump off that bridge when we get there. The RMI
parent can always remote-kill it.)

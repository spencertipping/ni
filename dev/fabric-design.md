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

ni('ni:/module')->new('/fabric')
  ->doc
  ->description(
    q[Abstractions to bridge the gaps between separate machines and processes.
      This module is designed to make it appear as though all resources are
      local, or at least can be referred to locally -- even when they belong to
      an external process (e.g. a Hadoop mapper) or another machine (e.g. a
      file over SSH). If we can bidirectionally communicate with a remote ni
      instance, then we can see its resources.],

    q[The fabric layer consists of a couple of things. First, we've got RMI
      support code that proxies any method call and return value(s) over a
      full-duplex data channel. Second, we have an async event loop that
      handles multiplexed IO using a single thread.],

    q[There are a couple of RMI-related invariants. First, objects never move;
      they are always owned by a particular ni instance, and are assumed to
      have unquantifiable dependencies that would be broken if we moved them
      using serialization. Second, all argument/return value data is immutable
      and stateless; to the extent that it depends on objects, these
      dependencies are indirected through a name table.]);

ni->extend("src/fabric/$_") for
  qw/ native /;

ni('ni:/lib/doc')->new('/fabric')
  ->description(
    q[Abstractions to bridge the gaps between separate machines and processes.
      This module is designed to make it appear as though all resources are
      local, or at least can be referred to locally -- even when they belong to
      an external process (e.g. a Hadoop mapper) or another machine (e.g. a
      file over SSH). If we can bidirectionally communicate with a remote ni
      instance, then we can see its resources.],

    q[At the RMI level, the abstraction involves sending a function to a remote
      ni instance; arguments are transported using simple serialization.
      ]
    );

ni->extend("src/fabric/$_") for ();

ni('ni:/lib/doc')->new('/fabric')
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
      handles multiplexed IO using a single thread.]);

ni->extend("src/fabric/$_") for
  qw/ remote
      rmi /;

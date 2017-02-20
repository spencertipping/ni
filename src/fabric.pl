ni('ni:/lib/doc')->new('/fabric')
  ->description(
    q[Abstractions to bridge the gaps between separate machines and processes.
      This module is designed to make it appear as though all resources are
      local, or at least can be referred to locally -- even when they belong to
      an external process (e.g. a Hadoop mapper) or another machine (e.g. a
      file over SSH). If we can bidirectionally communicate with a remote ni
      instance, then we can see its resources.]);

ni->extend("src/fabric/$_") for
  qw/ future
      perl /;

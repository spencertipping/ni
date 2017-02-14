ni('ni:/lib/doc')->new('/unix')
  ->description(
    q[Abstractions around UNIX stuff like file descriptors and child processes.
      We bypass as much Perl machinery as possible to keep things simple,
      particularly in terms of IO: FDs are just numbers so we don't have to
      contend with Perl close-on-deallocation issues or buffering. (Part of the
      reason we have to do this is that older Perls don't behave ideally:
      interrupted system calls sometimes look like EOF until Perl 5.14, for
      example.)],

    q[ni tries to be aware of the reality of file descriptors as a
      process-specific lens into the state of IO resources while providing a
      more functional interface. For our purposes, each IO resource is fully
      owned by a single process; a handoff involves closing the resource in the
      parent process and opening it under a possibly-different FD within the
      child. There are cases where IO resources have multiple readers or
      writers, but this is always mediated by a process rather than increasing
      the underlying reference count.],

    q[Forks are negotiated fully before they happen; the only exception is that
      it's assumed you want to be able to use stdio for a process, so any
      unmapped stdio descriptors are optimistically piped into the local FD
      namespace.]);

ni->extend("src/unix/$_")
  for qw/ io has_fd fd file cat str exec pid fifo pipeline /;

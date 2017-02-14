ni('ni:/lib/doc')->new('/unix')
  ->description(
    q[Abstractions around UNIX stuff like file descriptors and child processes.
      We bypass as much Perl machinery as possible to keep things simple,
      particularly in terms of IO: FDs are just numbers so we don't have to
      contend with Perl close-on-deallocation issues or buffering.]);

ni->extend("src/unix/$_")
  for qw/ io has_fd fd file cat str exec pid fifo pipeline /;

1;

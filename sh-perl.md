# `/bin/sh` -> Perl exec transition branch
Two things here. Fundamentally, we need a model where the Perl process is doing
the fork/pipe/exec stuff that would otherwise be done by `/bin/sh`. Perl is
stable doing this provided we're careful to avoid interrupted syscalls, which
happen when we set up a `SIGCHLD` handler and don't retry read/writes.

The second thing is the way we serialize a pipeline. We can't assume that every
system will have the same argv length limits, which means we need to somehow
pack the parent process's argv into the image itself before we send it
anywhere. This can work if we write a custom function and then use the
constant-space `--internal/eval X` (or similar) to invoke it. This, in turn,
means that we need an API to represent derivative images and library
collections.

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
collections. (Trivially we could just quote `@ARGV` and stash it into a wrapped
function. No sense in doing anything more complicated yet -- though we'll need
something marginally more complicated if we also want to handle lambdas.)

A lot of this branch comes down to re-engineering the argument parser.

**NB:** The point of the refactor is to avoid serializing the ni image into
each pipeline stage separately, and to simplify code sharing between ni and the
mapper code. (And to work around a double-free bug in some versions of
`/bin/sh`, though this doesn't seem to actually cause any problems.) However,
these benefits exist only for the Perl frontend; other languages will have all
of the same problems.

Actually this isn't quite true. If we can fork first and stdin-populate second,
we'll avoid the humongous heredoc chains we'd currently get, which should save
a ton of memory on ni's end as it constructs the compiled script (not to
mention the downstream savings if that script ends up being sent somewhere).
It's absolutely worth it: ni should fork and use callable ops.

## Pipeline monitors
While we're doing this, we might as well go ahead and figure out how to get
pipeline reporting in there. Ideally this would provide:

- Throughput/latency/bottleneck indicators
- Realtime data inspection/debugging (via UNIX socket maybe)
- Possibly other stuff later on (pluggable)

We get this interposition for free anytime we're launching a process that
requires a heredoc.

### ni-specific monitor message format
Writes are atomic, so something like this should work:

```
\n ni \0 ... \n
```

### STDERR as the pipe
This should be right. Then we can log it and support stuff like searching. If
we have a bunch of fds we can isolate each stderr stream to a specific process,
though I'm not sure what exactly this buys us. We also need to make sure stderr
doesn't block the monitor writers.

The monitors are trackable anyway because we can drop IDs into the closure
context. This means we need a fairly coherent view of the pipeline before we
commit to executing it.

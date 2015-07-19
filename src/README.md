# What's going on here
Internally, ni's command-line interface is just a translation layer on top of
Canard. Canard contains an abstraction for streams of values, and it has access
to POSIX libc calls to fork, exec, read, write, etc.

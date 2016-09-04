### Replace /bin/sh with perl
Using POSIX shell is more trouble than it's worth. Perl is a better runtime
environment because we can much more easily quote code; ni should produce
customized sub-images of itself when asked to compile a pipeline.

Running in-process isn't a good idea because then we lose the ability to filter
stderr (actually we don't; we can do the pipe-fork stuff to make it work just
fine).

### Process-consing design
Pipeline addition as consing, both for compilation and for runtime.

### Parser documentation
Rather than just returning values, parse states should include documentation,
info about the original input, and other intermediate stuff to explain what's
going on. Maybe also rejected parses?

### Fast partial JSON parsing
Something that uses regexes to parse JSON without allocating tons of memory.

### Optimized line processor
Right now we're slower than nfu, which is terrible. ni should be able to go
much faster.

### Optimization in general
Rewrite the SHA-1 pure-perl implementation to be something smaller than 30K.

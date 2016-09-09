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

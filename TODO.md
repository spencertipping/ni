### Code quoting abstraction
Right now we're quoting code using `sh [qw/perl -/], stdin => ...`, but this
doesn't make it easy to share libraries between the ni runtime and compiled ni
pipelines.

Not 100% sure about the ideal design, but I'm thinking some kind of
`context->eval(X)` abstraction where some contexts are runtime and others are
compiled.

### Benchmarking of various sorts
Startup time, operation throughput, etc (WIP: `dev/bench-X`)

### Parser documentation
Rather than just returning values, parse states should include documentation,
info about the original input, and other intermediate stuff to explain what's
going on. Maybe also rejected parses?

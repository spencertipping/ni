### Code quoting abstraction
Right now we're quoting code using `sh [qw/perl -/], stdin => ...`, but this
doesn't make it easy to share libraries between the ni runtime and compiled ni
pipelines.

Not 100% sure about the ideal design, but I'm thinking some kind of
`context->eval(X)` abstraction where some contexts are runtime and others are
compiled.

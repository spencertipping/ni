# Streams
A stream is a list of packets, a definition which unifies text and most binary
formats. Stream transformers are eager concatenative interpreters whose input
code is a list of packets. (A resolver is a function from interpreters to
interpreters, since the code changes under a different resolution function.)

EOF is an implied pseudo-command that happens when a stream will produce no
further values.

## Issues
- We need to prefer multibyte scanning for ignored-field efficiency, which
  requires that interpreters-as-operators optimize for byte instruction
  density.

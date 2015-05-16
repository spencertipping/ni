# ni: say "ni" to data
![ni!](http://spencertipping.com/ni.png)

MIT license as usual.

# Introduction
ni is a tool that understands how to move and transform most common types of
structured data. ni is self-compiling, requires no installation or
dependencies, and should run on any POSIX.2001-compliant system including
Linux, OSX, and most other Unices.

- [Optional dependencies](doc/optional-dependencies.md)

## Motivation
Your shell is the REPL of your system; it can process streams of arbitrary size
in rudimentary ways. It provides no abstraction capabilities, however; it's a
delegation language, not a computing language.

ni bridges that gap. It knows how to talk to SQL databases, run Hadoop jobs,
build and query Lucene indexes, speak HTTP/XML/HTML/JSON, plot things in R and
gnuplot, create and build one-off Maven projects, compile C programs, open
sockets, distribute work with SSH, and run one-liners in various programming
languages. It also knows how write custom binary protocols to connect the
pieces together in efficient ways, and it implements adaptive statistical
parsers for XML and JSON (which should converge to near-optimal average case
performance for your data).

In other words, ni is designed to be the easiest and fastest way to process
data.

# Documentation
## By example
- [Survival commands (read this first)](doc/survival.md)
- [Simple streaming](doc/simple-stream.md)
- [Core operators](doc/core-operators.md)
- [Addressing](doc/addressing.md)
- [Lambdas](doc/lambdas.md)

## Reference
- [Operators](doc/operators.md)

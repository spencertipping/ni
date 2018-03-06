Development option: find things (parsers, ops, etc) that have been defined but
have no documentation associated with them.

Development option: evaluate an expression within the `ni::` package, and print
the result. For example:

$ ni --dev/eval '3 + 4'
7

Development option: trace the ni::parse function and attempt to parse the
command line. Mostly useful for debugging.

Development option: trace the ni::parse function and evaluate the specified
parser on the rest of the command line arguments. For example:

$ ni --dev/parse-one 'parser "/qfn"' [gc]

Usage: ni --doc
Prints a list of all documentable types of things. For example, "parser" is one
such thing, and from there you can run `ni --doc/parser X`, where `X` is the
name of a parser. (`ni --doc/parser` will list all of them.)

Usage: ni --doc/alt [<alt-name>]
Print documentation for <alt-name> if specified; otherwise list all alts.

Usage: ni --doc/clispecial [<clispecial-name>]
Print documentation for <clispecial-name> if specified; otherwise list all clispecials.

Usage: ni --doc/context [<context-name>]
Print documentation for <context-name> if specified; otherwise list all contexts.

Usage: ni --doc/dsp [<dsp-name>]
Print documentation for <dsp-name> if specified; otherwise list all dsps.

Usage: ni --doc/long [<long-name>]
Print documentation for <long-name> if specified; otherwise list all longs.

Usage: ni --doc/meta_operator [<meta_operator-name>]
Print documentation for <meta_operator-name> if specified; otherwise list all meta_operators.

Usage: ni --doc/operator [<operator-name>]
Print documentation for <operator-name> if specified; otherwise list all operators.

Usage: ni --doc/parser [<parser-name>]
Print documentation for <parser-name> if specified; otherwise list all parsers.

Usage: ni --doc/short [<short-name>]
Print documentation for <short-name> if specified; otherwise list all shorts.

Usage: ni --explain normal-ni-options...
Describes the operators and meta-operators produced from the specified command
line. Meta-operators are unevaluated in this form.

Usage: ni --explain-meta normal-ni-options...
Describes the operators produced from the specified command line, after
evaluating all meta-operators. Each operator in the output corresponds to a
forked process in the pipeline.

Usage: ni --inspect [port=9200]
Runs a web interface that allows you to inspect ni's internal attributes,
defined operators, and grammar.

Usage: ni --internal/lib lib1 lib2 ... libN
Modifies the ni image in-place to include the specified libraries. See ni
//help/libraries for more information.

Internal option: causes ni to parse keys within its image and use those as the
list of operators to run. This is how all of ni's remoting is done; the
indirection prevents us from hitting any size limits on ARGV or ENV.

Usage: ni --js [port=8090]
Runs a web interface that allows you to visualize data streams. See ni
//help/visual for details.

Usage: ni --lib lib-dir normal-ni-options...
Preloads a library before running normally. ni does not self-modify on disk if
you do this, though the library will be available in remote contexts.

Usage: ni --run 'perl code' normal-ni-options...
Runs code before running normally.


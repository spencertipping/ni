# ni frontend functions: option parsing and compilation
# Supporting definitions are in ni/structure.sh, and meta/ni-option.sh for the
# metaprogramming used by home/conf.

# Grammar
# ni has a fairly complex command-line language consisting of the following
# elements:
#
#   quasifiles                  ni data.txt
#   short operators             ni -gcO
#   long operators              ni --groupuniq
#   variables                   ni @foo
#   checkpoints                 ni :foo.gz
#   outputs                     ni =foo.gz
#   branches                    ni { a -g , b -c }
#   branch lambdas              ni -A { a -g , b -c }
#   lambdas                     ni -A [ -st+1 ]
#   compilation contexts        ni octave[ 'xs = abs(fft(xs))' ]
#
# Those constructs in isolation would be easy to parse, but for ergonomic
# advantage ni also gives you some shorthands:
#
#   ni -A ^st+1                 # short quoting
#   ni -A [st+1]                # compact lambda form
#   ni -Ast+1                   # lambda coercion (context-sensitive)
#
#   ni @:foo.gz                 # variable "foo" and checkpoint foo.gz
#   ni @=foo.gz                 # same thing, but s/checkpoint/output/
#   ni @foo[ -gcO ]             # var from transformed stream
#   ni @foo[gcO]                # same
#   ni @foo^gcO                 # same
#
#   ni -gcX[ egrep 'foo' ]      # same as -gcX "egrep 'foo'"
#
#   ni -A/                      # specify null-lambda
#   ni -A:                      # specify identity lambda
#   ni -H://                    # three lambdas: id, null, null
#   ni -Hst+1//                 # [ -st+1 ], null, null
#
# Because of these shorthands, there isn't a construct for "optional lambda";
# all lambda arguments are required.

# Formal grammar
# It isn't possible to write a static EBNF grammar for ni because it depends on
# stuff in home/conf. However, by generalizing a bit:
#
#   start    ::= qfile | short | long | var | ckpt | output | branch | cmd
#   qfile    ::= [^-]...
#   short    ::= '-' short_op+
#   long     ::= '--' word
#   short_op ::= nullary | unary | lambda_op | ...      # <- home/conf
#   var      ::= '@' {':' | '='}? word explicit_lambda?
#   ckpt     ::= ':' qfile
#   output   ::= '=' qfile
#   branch   ::= '{' branch-case * '}'
#   cmd      ::= word '[ ' word* ' ]'
#
#   nullary         ::= char
#   unary           ::= char option
#   lambda_op       ::= char implicit_lambda
#   implicit_lambda ::= explicit_lambda | short_op+
#   explicit_lambda ::= '^' short_op+ | '[ ' start ' ]' | '[' short_op+ ']'



:

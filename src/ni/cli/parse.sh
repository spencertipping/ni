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
#
# 

:

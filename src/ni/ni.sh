# ni frontend core definitions: syntactic structures and multimethods
# See also meta/ni-option.sh for the metaprogramming used by conf.sh.

# Complex commands
defstruct lambda body           # [ ... ] or ^x
defstruct lambdafork body       # @[ ... ] or @^x
defstruct lambdaplus body       # -[ ... ]
defstruct lambdamix body        # -@[ ... ]
defstruct branch map            # { x ... , y ... , ... }
defstruct branchfork map        # @{ x ... , y ... , ... }
defstruct branchsub map         # -{ ... }
defstruct branchmix map         # -@{ ... }

# Pipeline compilation multimethods
defmulti to_options             # convert back to option list
defmulti compile                # compile to a shell command

# Option parsing
# Usage: ni_parse destination_var cli-options...
ni_parse() {
  cons ni_parse_stack   '' ''
  cons ni_parse_command '' ''
  ni_parse_r="$1"
  shift

  while [ $# -gt 0 ]; do
    ni_parse_closers=0
    ni_parse_push_one=t

    # First handle syntax cases.
    case "$1" in
    '[')   cons ni_command_stack lambda     $ni_command_stack ;;
    '@[')  cons ni_command_stack lambdafork $ni_command_stack ;;
    '-[')  cons ni_command_stack lambdaplus $ni_command_stack ;;
    '-@[') cons ni_command_stack lambdamix  $ni_command_stack ;;
    '{')   cons ni_command_stack branch     $ni_command_stack ;;
    '@{')  cons ni_command_stack branchfork $ni_command_stack ;;
    '-{')  cons ni_command_stack branchsub  $ni_command_stack ;;
    '-@{') cons ni_command_stack branchmix  $ni_command_stack ;;

    ']'|'}') ni_parse_push_one=
             ni_parse_closers=1 ;;

    # Now handle general options and quasifiles.
    *)
      ni_parse_push_one=
      if [ "x${1#--}" = "x$x" ]; then
        TODO long option
      elif [ "x${1#-}" = "x$x" ]; then
        TODO short option
      elif [ "x${1#^}" = "x$x" ]; then
        TODO short lambda
      else
        TODO quasifile $1
      fi
    esac

    [ "$ni_parse_push_one" = t ] && cons ni_parse_stack '' $ni_parse_stack

    while [ $ni_parse_closers -gt 0 ]; do
      uncons ni_command_this ni_command_stack $ni_command_stack
      uncons ni_parse_this   ni_parse_stack   $ni_parse_stack
      list_reverse ni_parse_this "$ni_parse_this"
      vector ni_parse_v "$ni_command_this" "$ni_parse_this"
      eval "cons \${ni_parse_stack}_h \$ni_parse_v \$${ni_parse_stack}_h"
      ni_parse_closers=$((ni_parse_closers - 1))
    done
  done
}

ni_compile() TODO ni_compile

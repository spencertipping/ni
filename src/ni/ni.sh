# ni frontend core definitions: syntactic structures and multimethods
# See also meta/ni-option.sh for the metaprogramming used by home/conf.

defstruct quasifile name

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
defmulti compile                # compile to a shell command
defmulti describe               # plain-English description

# Option parsing
# Usage: ni_parse destination_var $vector_ref
#
# $vector_ref comes from using "vector" to convert "$@" to a vector object.
# ni_parse will then consume the vector, which amounts to shifting it until
# it's empty.
ni_parse() {
  cons ni_parse_stack   '' ''
  cons ni_parse_command '' ''
  ni_parse_r="$1"
  shift

  while [ $# -gt 0 ]; do
    ni_parse_closers=0
    ni_parse_push_one=t
    ni_parse_option="$1"
    ni_parse_to_shift=1

    # First handle syntax cases.
    case "$ni_parse_option" in
    '[')   cons ni_parse_command lambda     $ni_parse_command ;;
    '@[')  cons ni_parse_command lambdafork $ni_parse_command ;;
    '-[')  cons ni_parse_command lambdaplus $ni_parse_command ;;
    '-@[') cons ni_parse_command lambdamix  $ni_parse_command ;;
    '{')   cons ni_parse_command branch     $ni_parse_command ;;
    '@{')  cons ni_parse_command branchfork $ni_parse_command ;;
    '-{')  cons ni_parse_command branchsub  $ni_parse_command ;;
    '-@{') cons ni_parse_command branchmix  $ni_parse_command ;;

    ']'|'}') ni_parse_push_one= ni_parse_closers=1 ;;

    # Now handle general options and quasifiles.
    *)
      ni_parse_push_one=

      # Look for any lambda prefixes, forking or not. These end as soon as
      # we've consumed all arguments for the quoted operator, so we increment
      # ni_parse_closers for each prefix.
      while [ "x${ni_parse_option#^}"  != "x$ni_parse_option" ] \
         || [ "x${ni_parse_option#@^}" != "x$ni_parse_option" ]; do
        cons ni_parse_stack '' $ni_parse_stack
        ni_parse_closers=$((ni_parse_closers + 1))
        if [ "x${ni_parse_option#@}" != "x$ni_parse_option" ]; then
          # Forking short lambda
          cons ni_parse_command lambdafork $ni_parse_command
        else
          # Non-forking short lambda
          cons ni_parse_command lambda $ni_parse_command
        fi
      done

      if [ "x${ni_parse_option#--}" != "x$ni_parse_option" ]; then
        # Long option parsing mode: retrieve the option's syntax and parse
        # additional arguments, stuffing them all into the specified option
        # structure.
        get ni_parse_option_ref $long_options "${ni_parse_option#--}"
        if [ -z "$ni_parse_option_ref" ]; then
          err "ni: unknown long option $ni_parse_option"
          return 1
        fi

        syntax ni_parse_syntax "$ni_parse_option_ref"
        fn     ni_parse_fn     "$ni_parse_option_ref"

        ni_parse_arguments=

        # After a long option, all arguments are expected to be provided as
        # separate options; i.e. we don't have any shorthands for
        # option-packing, since clearly the user cares nothing about concision.
        while [ -n "$ni_parse_syntax" ]; do
          substr ni_parse_syntax_x "$ni_parse_syntax" 0 1
          ni_parse_syntax="${ni_parse_syntax#?}"

          get ni_parse_optionsyntax "$option_syntaxes" "$ni_parse_syntax_x"
          mode ni_parse_optionmode $ni_parse_optionsyntax

          # TODO: can this work with lambdas? Is it sufficient to do a naive
          # forward-traversal to see where the lambda ends? The only problem is
          # that lambda delimiters are arguably subject to interpretation: we
          # don't know whether one is literal, or whether it's an argument to
          # an option that expects it.
          case "$ni_parse_syntax_x" in
          s|v) shift; cons ni_parse_arguments "$1" "$ni_parse_arguments" ;;
          D)   if string_matches "$2" "[0-9]"; then
                 shift
                 cons ni_parse_arguments "$1" "$ni_parse_arguments"
               fi ;;
          F)   if string_matches "$2" "[-.0-9]"; then
                 shift
                 cons ni_parse_arguments "$1" "$ni_parse_arguments"
               fi
          esac
        done
      elif [ "x${ni_parse_option#-}" != "x$ni_parse_option" ]; then
        TODO short option $ni_parse_option
      else
        TODO quasifile $ni_parse_option
      fi
    esac

    [ "$ni_parse_push_one" = t ] && cons ni_parse_stack '' $ni_parse_stack

    while [ $ni_parse_closers -gt 0 ]; do
      uncons ni_parse_the_cmd ni_parse_command $ni_parse_command
      uncons ni_parse_this    ni_parse_stack   $ni_parse_stack
      list_reverse ni_parse_this "$ni_parse_this"

      # TODO: no longer accurate; we need to apply a constructor here.
      vector ni_parse_v "$ni_parse_the_cmd" "$ni_parse_this"

      eval "cons \${ni_parse_stack}_h \$ni_parse_v \$${ni_parse_stack}_h"
      ni_parse_closers=$((ni_parse_closers - 1))
    done

    shift $ni_parse_to_shift
  done

  until [ -z "$ni_parse_stack" ]; do
    uncons "$ni_parse_r" ni_parse_stack "$ni_parse_stack"
  done
}

# Compiles a structure produced by ni_parse, returning the jit context to
# execute it. Usage:
# ni_compile dest_var $cons_structure
ni_compile() {
  TODO ni_compile
}

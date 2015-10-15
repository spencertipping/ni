# ni frontend functions: option parsing and compilation
# Supporting definitions are in ni/structure.sh, and meta/ni-option.sh for the
# metaprogramming used by home/conf.

# Option parsing
# Usage: ni_parse destination_var $vector_ref
#
# $vector_ref comes from using "vector" to convert "$@" to a vector object.
# ni_parse will then consume the vector, which amounts to shifting it until
# it's empty.
#
# The resulting structure is a vector of option defstructs.

ni_parse() {
  vector $1

  while lpop ni_parse_option $2; do
    case "$ni_parse_option" in
    # Closer: done with inner parse, so return
    ']'|'}')
      return 0
      ;;

    # Openers: parse inside, then add to vector. Delegate to ni_bracket_case to
    # select the constructing class.
    '['|'@['|'-['|'-@['|'{'|'@{'|'-{'|'-@{')
      ni_bracket_case ni_parse_b "$ni_parse_option"
      set -- "$1" "$2" "$ni_parse_b"
      ni_parse ni_parse_xs $2
      $3 ni_parse_obj $ni_parse_xs
      eval "rpush \$$1 \$ni_parse_obj"
      ;;

    # Long options
    --*)
      ni_parse_long_option ni_parse_obj ${ni_parse_option#--} $2
      eval "rpush \$$1 \$ni_parse_obj"
      ;;

    # Short options
    -*|^*|@^*)
      # Some explanation here. ni_parse is about establishing the context of an
      # argument, which is why we match so many cases for this branch.
      # ni_parse_short_options sorts out the lambda-notation for us by wrapping
      # the results (and intermediate ones; see doc/operators.md for some
      # examples of this). All we need to do is trim the leading -, if there is
      # one, from the option string so it sees exactly what it needs to.

      ni_parse_short_options ni_parse_objs ${ni_parse_option#-} $2
      eval "rappend \$$1 \$ni_parse_objs"
      ;;

    # Quasifiles
    *)
      quasifile ni_parse_obj "$ni_parse_option"
      eval "rpush \$$1 \$ni_parse_obj"
      ;;
    esac
  done
}

ni_bracket_case() {
  case "$2" in
  '[')   eval "$1=lambda"     ;;
  '@[')  eval "$1=lambdafork" ;;
  '-[')  eval "$1=lambdaplus" ;;
  '-@[') eval "$1=lambdamix"  ;;
  '{')   eval "$1=branch"     ;;
  '@{')  eval "$1=branchfork" ;;
  '-{')  eval "$1=branchsub"  ;;
  '-@{') eval "$1=branchmix"  ;;
  esac
}

# Compiles a structure produced by ni_parse, returning a jit context to execute
# it. Usage: ni_compile dest_var $parsed_vector

ni_compile() {
  TODO ni_compile
}

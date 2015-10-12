# ni frontend: main argument parser and shell script compiler
# See also meta/ni-option.sh for the metaprogramming used by conf.sh.

# Takes command-line options exactly as provided, and compiles a shell script
# to execute the result. The resulting shell script may refer to other
# generated programs, so it isn't network-portable. (ni as a whole, however,
# is.)
ni_compile() {
  TODO ni_compile
}

# Parses command-line options and returns (as $$1) a cons tree containing their
# structural representation. Each cons element is a vector of [fn args...].
# There are some special commands:
#
# [lambda (...)]                # [ -x ... ] or ^x
# [lambdafork (...)]            # @[ -x ... ] or @^x
# [lambdaplus (...)]            # -[ ... ]
# [lambdamix (...)]             # -@[ ... ]
#
# [branch {...}]                # { x ... , y ... , ... }
# [branchfork {...}]            # @{ x ... , y ... , ... }
# [branchsub {...}]             # -{ ... }
# [branchmix {...}]             # -@{ ... }
#
# Names of quasifiles are wrapped in string references

ni_parse_options() {
  ni_parse_options_r="$1"
  eval "$1="
  shift

  TODO ni_parse_options
}

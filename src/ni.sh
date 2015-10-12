# ni frontend: main argument parser and shell script compiler
# See also meta/ni-option.sh for the metaprogramming used by conf.sh.

# Takes command-line options exactly as provided, and compiles a shell script
# to execute the result. The resulting shell script may refer to other
# generated programs, so it isn't network-portable. (ni as a whole, however,
# is.)
ni_compile() {
  TODO ni_compile
}

# Parses a single command-line option, defining a named shell function to
# execute it and returning the remaining command-line. This function delegates
# to the command-line option table defined in conf.sh.
ni_cli_next() {
  TODO ni_cli_next
}

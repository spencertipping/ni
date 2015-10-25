# First-class functions
# NB: because all POSIX sh variables are global, these function-constructors
# typically build functions that are NOT recursion-safe.

# TODO: fix the local-variable and recursion thing using metaprogramming at
# this level.

fn() {
  cell $1 fn
  eval "\$$1() { $2$newline }"
}

partial() {
  partial_f=$1
  shift
  fn $partial_f "$@ \"\$@\""            # TODO: quoting
}

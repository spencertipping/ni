# Support for hashing arbitrary data through the jit-C interface
sha3() { sha3_setup; "$sha3_jit"; }
sha3_setup() {
  [ -n "$sha3_jit" ] && return
  module_get sha3_source bin/sha3.c
  sha3_jit="$(verb "$sha3_source" | jit_c)"
  unset sha3_source
}

setup_hooks="$setup_hooks${newline}sha3_setup"

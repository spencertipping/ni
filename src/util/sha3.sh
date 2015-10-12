# Support for hashing arbitrary data through the jit-C interface
sha3() { sha3_setup; "$sha3_jit"; }
sha3_setup() {
  [ -n "$sha3_jit" ] && return
  module_get sha3_source bin/sha3.c
  sha3_jit="$(verb "$sha3_source" | jit_c_base)"        # jit_c_base !!!
  unset sha3_source
}

setup_hooks="$setup_hooks${newline}sha3_setup"

# NB: no shutdown hook to free the jit context here, since the sha3 program
# should exist as long as ni maintains its tmpdir.

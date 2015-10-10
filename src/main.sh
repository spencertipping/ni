main() {
  tmpdir
  module_get hash_source bin/hash.c
  h=$(verb "$hash_source" | jit_c)
  $h
  tmpdir_free
}

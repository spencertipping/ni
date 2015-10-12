# JIT support for C99 programs
# Calling convention is like this, except that heredocs inside $() don't seem
# to be allowed:
#
# jit_program=$(jit_c [c99-option...] <<'EOF'
# #include <stdio.h>
# int main() {
#   printf("hello world\n");
#   return 0;
# }
# EOF
# )
#
# $jit_program "$@"             # to execute the program
# jit_c_free $jit_program       # to deallocate the program

# NB: this version is used for bootstrapping; specifically, we need to compile
# the sha3 program before we'll have any support for unified jit.
jit_c_base() {
  tmpfile jit_c_base_source .c
  cat > "$jit_c_base_source"
  c99 "$@" "$jit_c_base_source" -o "${jit_c_base_source%.c}"
  verb "${jit_c_base_source%.c}"
}

# This version is what you would normally use; it will reuse existing programs
# rather than recompiling them.
jit_c() {
  jit_c_source="$(canonical_file .c)"
  [ -x "${jit_c_source%.c}" ] \
    || c99 "$@" "$jit_c_source" -o "${jit_c_source%.c}"
  verb "${jit_c_source%.c}"
}

jit_c_free() tmpdir_rm "$1" "$1.c"

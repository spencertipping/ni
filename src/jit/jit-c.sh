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

jit_c_index=0
jit_c() {
  tmpdir
  jit_c_index=$((jit_c_index + 1))
  jit_c_source="$self_tmpdir/jit-c-$jit_c_index.c"
  cat > "$jit_c_source"
  c99 "$@" "$jit_c_source" -o "${jit_c_source%.c}"
  verb "${jit_c_source%.c}"
}

jit_c_free() rm "$1" "$1.c"

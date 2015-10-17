# JIT support for POSIX shell programs
#
# jit_program=$(jit_sh <<'EOF'
# echo "hello world $*"
# EOF
# )
#
# $jit_program "$@"             # to execute the program
# jit_sh_free $jit_program      # to deallocate the program

jit_sh() {
  tmpdir
  jit_sh_index=$((jit_sh_index + 1))
  jit_sh_source="$self_tmpdir/jit-sh-$jit_sh_index"
  { echo "#!/bin/sh"; cat; } > "$jit_sh_source"
  chmod 700 "$jit_sh_source"
  verb "$jit_sh_source"
}

jit_sh_free() tmpdir_rm "$1"

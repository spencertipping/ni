# Filesystem interop

# Usage: tmpdir destination_var
# Returns the original tmpdir if the destination var is already populated and
# exists.
tmpdir() {
  eval "[ -e \"\$${1:-self_tmpdir}/.this-is-a-ni-tmpdir\" ]" && return
  tmpdir_prefix="${TMPDIR:-/tmp}/ni-$$"
  tmpdir_index=0
  until mkdir "$tmpdir_prefix-$tmpdir_index" 2>/dev/null; do
    tmpdir_prefix=$((tmpdir_prefix + 1))
  done
  touch "$tmpdir_prefix-$tmpdir_index/.this-is-a-ni-tmpdir"
  eval "${1:-self_tmpdir}=\$tmpdir_prefix-\$tmpdir_index"
}

tmpdir_free() {
  [ $# -eq 0 ] && set -- "$self_tmpdir"
  if [ ! -e "$1/.this-is-a-ni-tmpdir" ]; then
    verb "ni: trying to clean up a tmpdir ($1)" \
         "    but this tmpdir doesn't appear to have been created by ni" \
         "    in this case, not doing anything" >&2
    return 1
  fi
  rm -r "$1"
}

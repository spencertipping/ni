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
    err "ni: trying to clean up a tmpdir ($1)" \
        "    but this tmpdir doesn't appear to have been created by ni" \
        "    in this case, not doing anything"
    return 1
  fi
  rm -r "$1"
}

# It's important to create a tmpdir at startup. If we don't, then a subprocess
# will; but subprocesses can't modify our variable-space so we won't see it
# (and therefore won't clean it up). The way around this is to prepend tmpdir
# to the list of start hooks so it happens before anything gets jitted.
setup_hooks="tmpdir$newline$start_hooks"
shutdown_hooks="$shutdown_hooks${newline}tmpdir_free"

# Exhume/inhume self to/from FS
# Usage: exhume existing-directory (populates self to directory)
exhume() {
  exhume_i=0
  exhume_old_ifs="$IFS"
  IFS="$newline"
  for exhume_m in $modules; do
    mkdir -p "$1/${exhume_m%/*}"
    eval "verb \"\$module_$exhume_i\"" > "$1/$exhume_m"
    exhume_i=$((exhume_i + 1))
  done
  IFS="$exhume_old_ifs"
}

# Usage: inhume exhumed-directory (populates directory to self)
# NB: just as exhume doesn't create one, this function doesn't remove the
# directory. Also, inhumed files are in arbitrary order except for boot.sh,
# which always goes first.
inhume() {
  module_index=0
  module boot.sh "$(cat "$1/boot.sh")"
  inhume_old_ifs="$IFS"

  # Always inhume meta stuff first
  IFS="$newline"
  for inhume_f in $(find "$1/meta" -type f); do
    IFS="$inhume_old_ifs"
    module "${inhume_f#$1}" "$(cat "$inhume_f")"
  done

  IFS="$newline"
  for inhume_f in $(find "$1" -type f); do
    IFS="$inhume_old_ifs"
    [ "$inhume_f" != "$1/boot.sh" ] \
      && [ "${inhume_f#$1/meta/}" = "$inhume_f" ] \
      && module "${inhume_f#$1}" "$(cat "$inhume_f")"
  done
}

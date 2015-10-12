# Creates uniquely-named files in the temporary directory

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

# Removes files, but makes sure they're in the temporary directory first. This
# is a sanity check to avoid blowing away random stuff on the filesystem, e.g.
# if a variable got cleared by mistake. Forwards the "-r" option if passed
# first.
tmpdir_rm() {
  tmpdir_rm_options=
  if [ "x$1" = "x-r" ]; then
    tmpdir_rm_options=-r
    shift
  fi
  for tmpdir_rm_f; do
    if [ "${tmpdir_rm_f#$self_tmpdir/}" != "$tmpdir_rm_f" ]; then
      rm "$tmpdir_rm_options" -- "$tmpdir_rm_f"
    else
      err "ni: attempting to remove \"$tmpdir_rm_f\", which is not" \
          "    located in a temp directory created by ni (in this case" \
          "    not doing anything)"
    fi
  done
}

# It's important to create a tmpdir at startup. If we don't, then a subprocess
# will; but subprocesses can't modify our variable-space so we won't see it
# (and therefore won't clean it up). The way around this is to prepend tmpdir
# to the list of start hooks so it happens before anything gets jitted.
setup_hooks="tmpdir$newline$start_hooks"
shutdown_hooks="$shutdown_hooks${newline}tmpdir_free"

# Usage: tmpfile dest_var [suffix]
tmpfile() {
  tmpdir
  tmpfile_i=0
  while [ -e "$self_tmpdir/$tmpfile_i$2" ]; do
    tmpfile_i=$((tmpfile_i + 1))
  done
  touch "$self_tmpdir/$tmpfile_i$2"
  eval "$1=\"\$self_tmpdir/\$tmpfile_i\$2\""
}

# Usage: dest_var=$(data-source | canonical_file [suffix])
# NB: uses disk-buffering, not memory-buffering. Does not, however, compress
# the file.
canonical_file() {
  tmpfile canonical_file_tmp $1
  cat > "$canonical_file_tmp"
  canonical_file_sha="$(sha3 < "$canonical_file_tmp")"
  mv "$canonical_file_tmp" "$self_tmpdir/$canonical_file_sha$1"
  verb "$self_tmpdir/$canonical_file_sha$1"
}

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
  IFS="$inhume_old_ifs"
}

# Writes the current image to the specified file. If the resulting image fails,
# prints the name of the tempfile where it is stored.
save() {
  save_file=$(self | canonical_file)
  save_state=$(self | sha3)
  save_check=$(sh "$save_file" --state)
  if [ "$save_state" = "$save_check" ]; then
    chmod 755 "$save_file"
    mv "$save_file" "$1"
  else
    err "ni: save failed; $save_state != $save_check"
    save_fail_file="${TMPDIR:-/tmp}/${save_file#$self_tmpdir/}"
    mv "$save_file" "$save_fail_file"
    verb "$save_fail_file"
    return 1
  fi
}

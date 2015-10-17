# Exhume/inhume self to/from FS
# Usage: exhume existing-directory (populates self to directory)
exhume() {
  exhume_i=0
  exhume_old_ifs="$IFS"
  IFS="$newline"
  for exhume_m in $modules; do
    [ "${exhume_m%/*}" = "$exhume_m" ] || mkdir -p "$1/${exhume_m%/*}"
    eval "verb \"\$module_$exhume_i\"" > "$1/$exhume_m"
    exhume_i=$((exhume_i + 1))
  done
  IFS="$exhume_old_ifs"
}

# Usage: inhume exhumed-directory (populates directory to self)
# NB: just as exhume doesn't create one, this function doesn't remove the
# directory. Also, inhumed files are in arbitrary order except for ni/boot.sh,
# which always goes first.
inhume() {
  module_index=0
  module ni/boot.sh "$(cat "$1/ni/boot.sh")"
  inhume_old_ifs="$IFS"
  inhume_dir="${1%/}"

  # Always inhume meta stuff first
  IFS="$newline"
  for inhume_f in $(find "$inhume_dir/ni/meta" -type f); do
    IFS="$inhume_old_ifs"
    module "${inhume_f#$inhume_dir/}" "$(cat "$inhume_f")"
  done

  IFS="$newline"
  for inhume_f in $(find "$inhume_dir" -type f); do
    IFS="$inhume_old_ifs"
    [ "$inhume_f" != "$inhume_dir/ni/boot.sh" ] \
      && [ "${inhume_f#$inhume_dir/ni/meta/}" = "$inhume_f" ] \
      && module "${inhume_f#$inhume_dir/}" "$(cat "$inhume_f")"
  done
  IFS="$inhume_old_ifs"
}

# Writes the current image to the specified file. If the resulting image fails,
# prints the name of the tempfile where it is stored.
save() {
  save_file=$(self | canonical_file)
  save_state=$(self | sha3)
  save_check=$(sh "$save_file" --internal-state)
  if [ "$save_state" = "$save_check" ]; then
    chmod 755 "$save_file"
    mv "$save_file" "$1"
  else
    err "ni: save failed; $save_state " \
        "    != $save_check" \
        "    (this means the new object failed to convince this one that " \
        "     it had managed to store everything and operate correctly)"
    save_fail_file="${TMPDIR:-/tmp}/${save_file#$self_tmpdir/}"
    mv "$save_file" "$save_fail_file"
    verb "$save_fail_file"
    return 1
  fi
}

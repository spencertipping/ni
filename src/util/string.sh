# String functions

# Substring function
# POSIX shell doesn't have an expansion for this (bash has ${var:start:len}),
# so we need to make one. This isn't especially fast, but it does work. All
# indexes start at zero.
#
# Usage: substr dest_var s start [length]
substr() {
  substr_start=$3
  substr_mask=
  substr_double=?
  while [ $substr_start -gt 0 ]; do
    [ $((substr_start & 1)) -eq 1 ] && substr_mask="$substr_mask$substr_double"
    substr_double="$substr_double$substr_double"
    substr_start=$((substr_start >> 1))
  done

  if [ -n "$4" ]; then
    eval "$1=\"\$(printf %.$4s \"\${2#\$substr_mask}\")\""
  else
    eval "$1=\"\${2#\$substr_mask}\""
  fi

  # These could be quite large, so go ahead and free some memory
  substr_double=
  substr_mask=
}

# Returns the number of characters at the beginning of a string that match a
# specified pattern.
# Usage: matching_chars dest_var "$s" [0-9a-f]
matching_chars() {
  # TODO: n lg n, not n²
  matching_chars_n=0
  matching_chars_s="$2"
  while [ "x${matching_chars_s#$3}" != "x$matching_chars_s" ]; do
    matching_chars_s="${matching_chars_s#?}"
    matching_chars_n=$((matching_chars_n + 1))
  done
  eval "$1=\$matching_chars_n"
}

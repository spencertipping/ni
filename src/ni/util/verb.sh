# Safe echo: works around the POSIX statement that "echo" is allowed to
# interpret things inside its arguments, e.g. escape sequences and -x options

verb() {
  for verb_arg; do
    printf "%s\n" "$verb_arg"
  done
}

err() verb "$@" >&2

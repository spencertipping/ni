# Safe echo: works around the POSIX statement that "echo" is allowed to
# interpret its arguments
verb() {
  for verb_arg; do
    printf "%s\n" "$verb_arg"
  done
}

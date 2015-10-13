# Support for todo-functions in code
TODO() {
  err "todo: $*"
  return 1
}

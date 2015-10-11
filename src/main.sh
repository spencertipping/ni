main_is_setup=
main_setup() {
  [ -n "$main_is_setup" ] && return
  eval "$meta_hooks"
  eval "$setup_hooks"
  main_is_setup=t
}

main() {
  main_setup

  err "todo"

  eval "$shutdown_hooks"
}

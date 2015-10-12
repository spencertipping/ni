# Main function, called automatically with all command-line arguments. The call
# is hard-coded in the image generator, so main() is a magic name.

main_is_setup=
main_setup() {
  [ -n "$main_is_setup" ] && return
  eval "$meta_hooks"
  eval "$setup_hooks"
  main_is_setup=t
}

main() {
  main_setup

  module_get conf_module home/conf
  eval "$conf_module"

  # Handle image-level special options
  case "$1" in
  --edit)
    shift
    err "ni: entering a shell to edit the current image" \
        "    any changes you make to files here will be reflected in ni" \
        "    and written back into $0 (assuming the result is able to" \
        "    function correctly; otherwise a tempfile will be saved)"
    repl_stateless "$@"
    main_save_result="$(save "$0")"
    if [ -n "$main_save_result" ]; then
      err "ni: the current image failed to save itself: preserving state;" \
          "    the broken image is saved as $main_save_result."
    fi
    ;;

  --install)
    shift
    main_save_dest="${1:-$HOME/bin/ni}"
    [ "${main_save_dest#/}" = "$main_save_dest" ] \
      && main_save_dest="$PWD/$main_save_dest"
    main_save_dir="${main_save_dest%/*}"
    main_save_i=0
    mkdir -p "$main_save_dir"
    if [ -e "$main_save_dest" ]; then
      while [ -e "$main_save_dest$main_save_i" ]; do
        main_save_i=$((main_save_i + 1))
      done
      save "$main_save_dest$main_save_i" || return $?
    else
      save "$main_save_dest" || return $?
      [ $# -gt 0 ] || err "ni: installed image at $main_save_dest"
    fi
    ;;

  --init)
    if [ -e ni ]; then
      err "ni: image already exists here; not overwriting"
    else
      save ni && err "ni: created image at ./ni" \
              || return $?
    fi
    ;;

  --repl)  shift; repl_sh ;;
  --self)  shift; self "$@" ;;
  --sha3)  shift; sha3 ;;
  --state) shift; self "$@" | sha3 ;;

  *)
    ni_compile main_ni "$@" && "$main_ni"
    ;;
  esac

  eval "$shutdown_hooks"
}

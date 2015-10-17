# Main function, called automatically with all command-line arguments. The call
# is hard-coded in the image generator, so main() is a magic name.

main_is_setup=
main_setup() {
  [ -n "$main_is_setup" ] && return
  eval "$meta_hooks"
  eval "$setup_hooks"
  main_is_setup=t
}

make_home() {
  # ni isn't distributed with a populated home/ directory because then upgrades
  # would overwrite your stuff. Instead, ni creates home/ if home/conf doesn't
  # exist.
  if ! module_get make_home_conf home/conf; then
    make_home_oldifs="$IFS"
    IFS="$newline"
    make_home_i=0
    for make_home_m in $modules; do
      if [ "${make_home_m#home-template/}" != "$make_home_m" ]; then
        eval "module \"home/\${make_home_m#home-template/}\" \
                     \"\$module_$make_home_i\""
      fi
      make_home_i=$((make_home_i + 1))
    done
  fi
}

interactive_edit() {
  # Opens the user's preferred editor on a file, asking the user about their
  # editor preference if we're unsure.
  module_get interactive_edit_editor home/editor
  if [ -z "$interactive_edit_editor" ]; then
    # Not sure what to use; ask user and save their preference
    interactive_edit_editor="${EDITOR:-$VISUAL}"
    until "$interactive_edit_editor" "$@"; do
      err "ni: didn't find anything in \$EDITOR or \$VISUAL;" \
          "    what is your preferred text editor?" \
          "> "
      read interactive_edit_editor
    done
    module home/editor "$interactive_edit_editor"
  else
    "$interactive_edit_editor" "$@"
  fi
}

main() {
  main_setup

  # Handle image-level special options
  case "$1" in
  --edit)
    shift
    make_home
    err "ni: entering a shell to edit the current image" \
        "    any changes you make to files here will be reflected in ni" \
        "    and written back into $0 (assuming the result is able to" \
        "    function correctly; otherwise a tempfile will be saved)" \
        "" \
        "    exit the shell to save your changes and return" \
        ""
    repl_stateless "$@"
    save "$0"
    ;;

  --conf|--config|--configure)
    shift
    make_home
    tmpdir main_self_dir \
      && exhume "$main_self_dir" \
      && interactive_edit "$@" "$main_self_dir/home/conf" \
      && inhume "$main_self_dir" \
      && rm -r "$main_self_dir"

    save "$0"
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

  # FS interop
  --unpack|--expand|--exhume) shift; mkdir "$1" && exhume "$1" ;;
  --use|--intern|--inhume)    shift; inhume "$1" && save "$0" ;;

  # Internal options for the build process and debugging
  --internal-repl)  shift; repl_sh "$@" ;;
  --internal-self)  shift; self "$@" ;;
  --internal-sha3)  shift; sha3 ;;
  --internal-state) shift; self "$@" | exec "$sha3" ;;

  # Normal invocation: parse CLI and run data pipeline
  *)
    make_home
    require home/conf

    vector main_options "$@"
    str s1 $main_options
    verb "options = $s1"
    ni_parse main_parsed "$main_options"
    str s2 $main_parsed
    verb "parsed  = $s2"
    ;;
  esac

  eval "$shutdown_hooks"
}

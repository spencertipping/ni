# Retrieves a module's text by name
# Usage: module_get destination_var module/name
# Does nothing if the variable is already set, which makes it possible to use
# this function repeatedly without a performance hit.
module_get() {
  eval "[ -n \"\$$1\" ]" && return
  module_get_old_ifs="$IFS"
  IFS="$newline"
  module_get_i=0
  for module_get_name in $modules; do
    if [ "$2" = "$module_get_name" ]; then
      eval "$1=\"\$module_$module_get_i\""
      IFS="$module_get_old_ifs"
      return 0
    fi
    module_get_i=$((module_get_i + 1))
  done
  IFS="$module_get_old_ifs"
}

# Evaluates the specified module as shell code.
require() {
  module_get require_code "$1"
  eval "$require_code"
}

# Prints a representation of this object to stdout. If invoked with --no-main
# as the first option, no call to main() will be generated at the end of the
# image. If invoked with --no-boot, boot.sh will be serialized as a normal
# module rather than treated specially (which will produce a broken image, but
# may be useful for serialization purposes).
self() {
  self_main='main "$@"'
  self_boot=t
  while [ $# -gt 0 ]; do
    [ "$1" = "--no-main" ] && self_main=
    [ "$1" = "--no-boot" ] && self_boot=
    shift
  done

  main_setup
  [ -n "$self_boot" ] \
    && verb "#!/bin/sh" \
            "# <body style='display:none'><script type='ni' id='self'>" \
            "# Self-modifying ni image: https://github.com/spencertipping/ni" \
            "module_0='$module_0'" \
            'eval "$module_0"'

  self_old_ifs="$IFS"
  IFS="$newline"
  self_i=0
  for self_m in $modules; do
    if [ -z "$self_boot" ] || [ "$self_m" != boot.sh ]; then
      eval "self_marker=\"\$(verb \"\$module_$self_i\" | exec "$sha3")\""
      verb "module '$self_m' <<'$self_marker'"
      eval "verb \"\$module_$self_i\""
      verb "$self_marker"
    fi
    self_i=$((self_i + 1))
  done
  IFS="$self_old_ifs"
  verb "# </""script>" "$self_main"
}

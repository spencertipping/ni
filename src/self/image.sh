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

# Prints a representation of this object to stdout.
self() {
  main_setup
  verb "#!/bin/sh" \
       "# <body style='display:none'><script type='ni' id='self'>" \
       "# Self-modifying ni image: https://github.com/spencertipping/ni" \
       "module_0='$module_0'" \
       'eval "$module_0"'

  self_old_ifs="$IFS"
  IFS="$newline"
  self_i=0
  for self_m in $modules; do
    if [ $self_m != boot.sh ]; then
      eval "self_marker=\"\$(echo \"\$module_$self_i\" | sha3)\""
      verb "module '$self_m' <<'$self_marker'"
      eval "verb \"\$module_$self_i\""
      verb "$self_marker"
    fi
    self_i=$((self_i + 1))
  done
  IFS="$self_old_ifs"
  verb "# </""script>" 'main "$@"'
}

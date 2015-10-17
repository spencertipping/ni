newline="$(printf "\\n ")" && newline="${newline% }"
module() {
  [ $# -eq 2 ] && module_v="$2" || module_v="$(cat)"
  eval "module_$module_index=\"\$module_v\""
  [ ${1%.sh} = $1 ] || eval "eval \"\$module_v\"" || echo "in module $1" >&2
  modules="$modules$newline$1"
  module_index=$((module_index + 1))
}
module_index=1
modules=ni/boot.sh
meta_hook() meta_hooks="$meta_hooks$newline$(cat)"

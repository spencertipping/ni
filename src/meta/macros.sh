# Useful macro definitions that aren't library-specific
let_level=0
let() {
  verb "$@"
  let_level=$((let_level + 1))
  macroexpand
  let_level=$((let_level - 1))
  for let_name; do
    verb "unset ${let_name%%=*}"
  done
}

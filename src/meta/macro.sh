# AOT shell macroexpansion
# This is actually really simple; we just use %() as an AOT macroexpansion
# indicator and have a awk script that shell-quotes stuff accordingly. The
# exact structure is like this:
#
# %(macro-name [args...]                # passed in as $1, etc
#   ....)                               # body quoted as a hardquoted heredoc
#
# Paren counting works just like it does in POSIX shell: quoted parens are
# ignored. Sub-macroexpansion contexts aren't allowed.

macroexpand() {
  module_get macroexpand_awk meta/macroexpand.awk
  eval "$(awk -e "$macroexpand_awk")"
}

main() {
  macroexpand
}

# REPL environment for testing things and editing the image
# Explodes the image into the filesystem, cd's there, and runs a sub-shell
# that's prepopulated with all of ni's shell state. This means the subshell
# will be a POSIX shell, not bash, ksh, or csh.
#
# If you want your preferred shell in an exploded state directory (but without
# in-memory state), you can use repl_stateless.

repl_sh() {
  sh_repl_self_dir="$(self | sha3)"

}

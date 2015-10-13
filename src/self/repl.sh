# REPL environment for testing things and editing the image
# Explodes the image into the filesystem, cd's there, and runs a sub-shell
# that's prepopulated with all of ni's shell state. This means the subshell
# will be a POSIX shell, not bash, ksh, or csh.
#
# If you want your preferred shell in an exploded state directory (but without
# in-memory state), you can use repl_stateless.

repl_sh() {
  tmpdir repl_sh_self_dir
  repl_sh_state="$(self --no-main | jit_sh)"
  exhume "$repl_sh_self_dir" \
    && (cd "$repl_sh_self_dir/home" || cd "$repl_stateless_self_dir"
        cat "$repl_sh_state" \
            "$(verb main_setup | canonical_file)" \
            - \
            "$(verb "eval \"\$shutdown_hooks\"" | canonical_file)" \
        | exec sh) \
    && jit_sh_free "$repl_sh_state" \
    && inhume "$repl_sh_self_dir" \
    && rm -r "$repl_sh_self_dir"
}

repl_stateless() {
  tmpdir repl_stateless_self_dir
  exhume "$repl_stateless_self_dir" \
    && (cd "$repl_stateless_self_dir/home" || cd "$repl_stateless_self_dir"
        export PS1="ni$ "
        export PROMPT="ni$ "
        exec "${SHELL:-bash}" || exec sh) \
    && inhume "$repl_stateless_self_dir" \
    && rm -r "$repl_stateless_self_dir"
}

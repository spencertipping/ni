# Cons cell primitive
meta_hook <<'EOF'
defstruct --no-str cons h t
EOF

# NB: non-standard calling convention. This is used only within sh functions
# (i.e. what it does isn't expressible in lisp mode).
uncons() eval "$1=\"\$${3}_h\"; $2=\"\$${3}_t\""

cons_str() {
  cons_str_x="$2"
  cons_str_s=
  while [ -n "$cons_str_x" ]; do
    h cons_str_h $cons_str_x
    t cons_str_x $cons_str_x
    set -- "$1" "$cons_str_x" "$cons_str_s"     # for recursion
    str cons_str_h $cons_str_h
    cons_str_s="$3 $cons_str_h"
    cons_str_x="$2"
  done
  eval "$1=\"(\${cons_str_s# })\""
}

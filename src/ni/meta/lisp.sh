# Lisp in POSIX shell ... because we can (I think).
# Not a completely introspective form of lisp; this is just a way to get around
# sh's tragic lack of local variables so I can write parser combinators without
# too much effort.

# Reader
lisp_convert() sed 's/\([^$]\|^\)\([][(){}]\)/\1 \2 /g'
lisp_read() {
  lisp_read_dest=$1
  shift
  cons lisp_read_r '' ''
  for lisp_read_x; do
    if [ -z "${lisp_read_x#[[(\{]}" ]; then
      cons lisp_read_r '' $lisp_read_r
    elif [ -z "${lisp_read_x#[])\}]}" ]; then
      uncons lisp_read_head lisp_read_r $lisp_read_r
      h lisp_read_tailhead $lisp_read_r
      list_reverse lisp_read_head $lisp_read_head
      if [ "$lisp_read_x" = "]" ]; then
        vec lisp_read_head $lisp_read_head
      elif [ "$lisp_read_x" = "}" ]; then
        # Go back and stringify the keys into sh primitives.
        hashmap lisp_read_map
        while [ -n "$lisp_read_head" ]; do
          uncons lisp_read_k lisp_read_head $lisp_read_head
          uncons lisp_read_v lisp_read_head $lisp_read_head
          str lisp_read_k $lisp_read_k
          assoc $lisp_read_map $lisp_read_k $lisp_read_v
        done
        lisp_read_head=$lisp_read_map
      fi
      cons ${lisp_read_r}_h $lisp_read_head $lisp_read_tailhead
    else
      h lisp_read_head $lisp_read_r
      atom lisp_read_cell $lisp_read_x
      cons ${lisp_read_r}_h $lisp_read_cell "$lisp_read_head"
    fi
  done
  h $lisp_read_dest $lisp_read_r
  eval "list_reverse $lisp_read_dest \$$lisp_read_dest"
}

# Evaluator
# NB: no function table, since functions are compiled to sh. Globals are
# self-named, e.g. (def foo 5) is referenced as $foo.
meta_hook <<'EOF'
hashmap lisp_macros
hashmap lisp_specials
hashmap fn_arity
EOF

# Usage: lisp_eval dest_var $reader_output
lisp_eval() {
  :
}

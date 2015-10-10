# Lisp in POSIX shell ... because we can.

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
          uncons lisp_read_head_k lisp_read_head $lisp_read_head
          uncons lisp_read_head_v lisp_read_head $lisp_read_head
          str lisp_read_head_k $lisp_read_head_k
          assoc $lisp_read_map $lisp_read_head_k $lisp_read_head_v
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

# Compiler
# This lisp uses a TCL-style evaluation model; that is, () is interpolated but
# words themselves are assumed to be self-representing.


# Vector data structure

meta_hook <<'EOF'
defmulti n
defmulti nth
EOF

# This is roughly what defstruct generates, though this is a bit more
# complicated because it supports variable arity.
vector() {
  vector_r=$1
  shift
  cell vector_cell vector
  eval "${vector_cell}_n=0"
  push $vector_cell "$@"
  eval "$vector_r=\$vector_cell"
}

vector_gc() {
  vector_gc_r=$1
  eval "vector_gc_n=\$${2}_n"
  vector_gc_i=0
  vector_gc_s=
  while [ $vector_gc_i -lt $vector_gc_n ]; do
    eval "vector_gc_s=\"\$vector_gc_s \$${2}_$vector_gc_i\""
    vector_gc_i=$((vector_gc_i + 1))
  done
  eval "$vector_gc_r=\"\$vector_gc_s\""
}

vector_n()   eval "$1=\"\$${2}_n\""
vector_nth() eval "$1=\"\$${2}_$3\""

vector_str() {
  vector_str_i=0
  vector_str_s=''
  n vector_str_n $2
  while [ $vector_str_i -lt $vector_str_n ]; do
    set -- "$1" "$2" "$vector_str_s" "$vector_str_i" "$vector_str_n"
    nth vector_str_x $2 $vector_str_i
    str vector_str_x $vector_str_x
    vector_str_s="$3 $vector_str_x"
    vector_str_i=$(($4 + 1))
    vector_str_n=$5
  done
  eval "$1=\"[\${vector_str_s# }]\""
}

# Converts a list reference to a vector, unlike (vector) above which makes one
# out of its shell arguments.
vec() {
  vec_r=$1
  vec_l=$2
  shift 2
  set --
  while [ -n "$vec_l" ]; do
    uncons vec_h vec_l $vec_l
    set -- "$@" "$vec_h"
  done
  vector "$vec_r" "$@"
}

# Pushes one or more objects onto the end of a vector, modifying it in place.
push() {
  push_v=$1
  n push_i $push_v
  shift
  for push_x; do
    eval "${push_v}_$push_i=\"\$push_x\""
    eval "${push_v}_n=\$((${push_v}_n + 1))"
    shift
  done
}

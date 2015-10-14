# Vector data structure

# sh won't let us overload "shift", so I decided to prefer the (somewhat
# un-idiomatic) directional push/pop mnemonics below.
meta_hook <<'EOF'
defmulti   n nth vec
defmulti   lpop  rpop
defmulti 1 lpush rpush
EOF

primitive_vec() {
  if [ $# -eq 2 ] && [ ${#2} -eq 0 ]; then
    vector "$1"
  else
    vector "$@"
  fi
}

# This is roughly what defstruct would have generated, though this is a bit
# more complicated because it supports variable arity.
vector() {
  vector_r=$1
  shift
  cell vector_cell vector
  eval "${vector_cell}_n=0 ${vector_cell}_shift=0"
  rpush $vector_cell "$@"
  eval "$vector_r=\$vector_cell"
}

vector_gc() {
  vector_gc_r=$1
  eval "vector_gc_n=\$${2}_n vector_gc_i=\$${2}_shift"
  vector_gc_s=
  while [ $vector_gc_i -lt $vector_gc_n ]; do
    eval "vector_gc_s=\"\$vector_gc_s \$${2}_$vector_gc_i\""
    vector_gc_i=$((vector_gc_i + 1))
  done
  eval "$vector_gc_r=\"\$vector_gc_s\""
}

vector_n() eval "$1=\$((${2}_n - ${2}_shift))"

vector_nth() {
  eval "vector_nth_i=\$${2}_shift"
  eval "$1=\"\$${2}_$(($3 + vector_nth_i))\""
}

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
# out of its shell arguments. nil is handled by primitive_vec.
cons_vec() {
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
vector_rpush() {
  vector_rpush_v=$1
  shift
  for vector_rpush_x; do
    eval "vector_rpush_i=\$${vector_rpush_v}_n"
    eval "${vector_rpush_v}_$vector_rpush_i=\"\$vector_rpush_x\" " \
         "${vector_rpush_v}_n=\$((${vector_rpush_v}_n + 1))"
    shift
  done
}

# Unshifts one or more elements onto the beginning of a vector. They're
# unshifted in specified order; e.g. vector_lpush $v 1 2 3 results in [1 2 3
# ...].
vector_lpush() {
  vector_lpush_v=$1
  shift
  eval "${vector_lpush_v}_shift=\$((${vector_lpush_v}_shift - $#))"
  eval "vector_lpush_i=\$${vector_lpush_v}_shift"
  for vector_lpush_x; do
    eval "${vector_lpush_v}_$vector_lpush_i=\"\$vector_lpush_x\""
    vector_lpush_i=$((vector_lpush_i + 1))
  done
}

# Pops an object from the beginning of the vector, modifying the vector in
# place. Usage:
# vector_lpop object_var $vector_ref
#
# Returns with exit code 1 and does nothing else if there are no more elements
# to be popped.
vector_lpop() {
  eval "[ \$${2}_shift -lt \$${2}_n ]" || return 1
  eval "vector_lpop_i=\$${2}_shift"
  eval "$1=\"\$${2}_$vector_lpop_i\""
  unset ${2}_$vector_lpop_i
  eval "${2}_shift=\$((vector_lpop_i + 1))"
}

vector_rpop() {
  eval "[ \$${2}_n -gt \$${2}_shift ]" || return 1
  eval "vector_rpop_n=\$((${2}_n - 1))"
  eval "$1=\$${2}_$vector_rpop_n"
  unset ${2}_$vector_rpop_n
  eval "${2}_n=\$vector_rpop_n"
}

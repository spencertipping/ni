# Lisp in POSIX shell ... because we can.

# Now we need core primitives, starting with cons cells. All of these use the
# first argument as the return value because we want to avoid creating
# subshells.
cell_index=0
cell() {
  eval "$1=$2_\$cell_index"
  cell_index=$((cell_index + 1))
  [ "${cell_index%000}" = "$cell_index" ] || gc
}

# Defines a named structure with a constructor and GC visitor.
defined_structs=
defstruct() {
  defined_structs="$defined_structs $1"
  defstruct_name=$1
  defstruct_visitor="gc_$1() eval \"\$1=\\\""
  defstruct_ctor="$1() { cell ${1}_cell $1; eval \""
  shift

  defstruct_i=2
  for defstruct_field; do
    defstruct_ctor="$defstruct_ctor$newline\${${defstruct_name}_cell}_$defstruct_field=\$$defstruct_i"
    defstruct_visitor="$defstruct_visitor \${2}_$defstruct_field"
    eval "$defstruct_field() eval \"\$1=\\\"\\\$\${2}_$defstruct_field\"\\\""
    eval "${defstruct_field}_set() eval \"\${1}_$defstruct_field=\\\"\\\$2\\\"\""
    defstruct_i=$((defstruct_i + 1))
  done

  eval "$defstruct_ctor$newline\$1=\$${defstruct_name}_cell\"; }"
  eval "$defstruct_visitor\\\"\""
}

# Defines a type-prefixed multimethod; e.g. "defmulti str" would expand into a
# call to cons_str "$@" if called with $2 as a cons cell.
defmulti() eval "$1() \${2%_*}_$1 \"\$@\""

defstruct cons h t
defstruct string x
defstruct atom   x

# This is roughly what defstruct generates, though this is a bit more
# complicated because it supports variable arity.
vector() {
  vector_r=$1
  shift
  cell vector_cell vector
  eval "${vector_cell}_n=0"
  for vector_x; do
    eval "${vector_cell}_$vector_cell_n=\"\$vector_x\"
          $vector_cell_n=\$((vector_cell_n + 1))"
  done
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

n()   eval "$1=\"\$${2}_n\""
nth() eval "$1=\"\$${2}_$3\""

defmulti str

_str()       eval "$1=nil"
string_str() eval "$1=\"\$${2}_x\""
atom_str()   eval "$1=\"\$${2}_x\""

cons_str() {
  cons_str_x="$2"
  cons_str_s=''
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

# A few list functions
list_reverse() {
  if [ -n "$2" ]; then
    h list_reverse_h $2
    t list_reverse_t $2
    cons list_reverse_l $list_reverse_h $3
    list_reverse "$1" "$list_reverse_t" $list_reverse_l
  else
    eval "$1=\$3"
  fi
}

vec() {
  vec_r=$1
  vec_l=$2
  shift 2
  set --
  while [ -n "$vec_l" ]; do
    h vec_l_h $vec_l
    t vec_l $vec_l
    set -- "$@" "$vec_l_h"
  done
  vector "$vec_r" "$@"
}

# Reader
lisp_convert() sed 's/\([^$]\|^\)\([()\[\]{}]\)/\1 \2 /g'
lisp_read() {
  lisp_construct_dest=$1
  shift
  cons lisp_construct_r '' ''
  for lisp_construct_x; do
    if [ -z "${lisp_construct_x#[\[(]}" ]; then
      cons lisp_construct_r '' $lisp_construct_r
    elif [ -z "${lisp_construct_x#[\[(]}" ]; then
      h lisp_construct_head $lisp_construct_r
      t lisp_construct_r $lisp_construct_r
      h lisp_construct_tailhead $lisp_construct_r
      list_reverse lisp_construct_head $lisp_construct_head
      if [ "$lisp_construct_x" = "]" ]; then
        vec lisp_construct_head $lisp_construct_head
      fi
      cons ${lisp_construct_r}_h $lisp_construct_head $lisp_construct_tailhead
    else
      h lisp_construct_head $lisp_construct_r
      atom lisp_construct_cell $lisp_construct_x
      cons ${lisp_construct_r}_h $lisp_construct_cell "$lisp_construct_head"
    fi
  done
  h $lisp_construct_dest $lisp_construct_r
  eval "list_reverse $lisp_construct_dest \$$lisp_construct_dest"
}

# Compiler
# This lisp uses a TCL-style evaluation model; that is, () is interpolated but
# words themselves are assumed to be self-representing.

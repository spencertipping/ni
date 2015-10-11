# Data structure metaprogramming
cell_index=0
cell() {
  eval "$1=_$2_\$cell_index"
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
defmulti() eval "$1() {
                   multi_${1}_type=\${2%_*}
                   \${multi_${1}_type#_}_$1 \"\$@\"
                 }"

# Default multimethods available for all structures
defmulti str

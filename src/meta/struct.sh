# Data structure metaprogramming
cell_index=0
cell() {
  eval "$1=_$2_\$cell_index"
  cell_index=$((cell_index + 1))
  [ "${cell_index%000}" = "$cell_index" ] || TODO gc
}

# Defines a named structure with a constructor and GC visitor.
defined_structs=
defstruct() {
  defstruct_emit_ctor=t
  defstruct_emit_gc=t
  defstruct_emit_accessors=t
  defstruct_emit_str=t

  while [ "x${1#--}" != "x$1" ]; do
    case "$1" in
    --no-ctor) defstruct_emit_ctor= ;;
    --no-gc)   defstruct_emit_gc= ;;
    --no-acc*) defstruct_emit_accessors= ;;
    --no-str)  defstruct_emit_str= ;;
    esac
    shift
  done

  defined_structs="$defined_structs $1"
  defstruct_name=$1
  defstruct_str="${1}_str() eval \"\$1=\\\"<$1"
  defstruct_visitor="${1}_gc() eval \"\$1=\\\""
  defstruct_ctor="$1() { cell ${1}_cell $1; eval \""
  shift

  defstruct_i=2
  for defstruct_field; do
    defstruct_ctor="$defstruct_ctor$newline\${${defstruct_name}_cell}_$defstruct_field=\$$defstruct_i"
    defstruct_visitor="$defstruct_visitor \${2}_$defstruct_field"
    defstruct_str="$defstruct_str $defstruct_field=\\\$\${2}_$defstruct_field"
    if [ -n "$defstruct_emit_accessors" ]; then
      eval "$defstruct_field() eval \"\$1=\\\"\\\$\${2}_$defstruct_field\"\\\""
      eval "${defstruct_field}_set() eval \"\${1}_$defstruct_field=\\\"\\\$2\\\"\""
    fi
    defstruct_i=$((defstruct_i + 1))
  done

  [ -n "$defstruct_emit_ctor" ] && eval "$defstruct_ctor$newline\$1=\$${defstruct_name}_cell\"; }"
  [ -n "$defstruct_emit_str"  ] && eval "$defstruct_str>\\\"\""
  [ -n "$defstruct_emit_gc"   ] && eval "$defstruct_visitor\\\"\""
}

# Defines a type-prefixed multimethod; e.g. "defmulti str" would expand into a
# call to cons_str "$@" if called with $2 as a cons cell.
#
# If the object in question is not a reference (i.e. it's a bare string), then
# it is its own string representation.
defmulti() eval "$1() {
                   multi_${1}_type=\${2%_*}
                   [ \"\${multi_${1}_type#_}\" != \"\$multi_${1}_type\" ] \
                     && \${multi_${1}_type#_}_$1 \"\$@\" \
                     || primitive_str \"\$@\"
                 }"

# Default multimethods available for all structures
defmulti str
primitive_str() eval "$1=\"\$2\""

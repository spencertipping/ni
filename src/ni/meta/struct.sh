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
#
# Some functions don't return things, while others return multiple values. To
# deal with this, defmulti accepts an optional number before the multimethod
# name to indicate the number of the argument whose type to use for dispatch.
defmulti() {
  defmulti_n=2
  if string_matches "$1" [0-9]; then
    defmulti_n=$1
    shift
  fi

  for defmulti_name; do
    eval "$defmulti_name() {
      multi_${defmulti_name}_type=\${$defmulti_n%_*}
      if [ \"\${multi_${defmulti_name}_type#_}\" != \"\$multi_${defmulti_name}_type\" ]; then
        \${multi_${defmulti_name}_type#_}_$defmulti_name \"\$@\"
      else
        primitive_$defmulti_name \"\$@\"
      fi
    }"
  done
}

# Default multimethods available for all structures
meta_hook <<'EOF'
defmulti str
EOF

primitive_str() eval "$1=\"\$2\""

pr() {
  for pr_x; do
    str pr_s "$pr_x"
    verb "$pr_s"
  done
}

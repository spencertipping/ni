#!/bin/sh
# <body style='display:none'><script type='ni' id='self'>
# Self-modifying ni image: https://github.com/spencertipping/ni
module_0='newline="$(printf "\\n ")" && newline="${newline% }"
module() {
  [ $# -eq 2 ] && module_v="$2" || module_v="$(cat)"
  eval "module_$module_index=\"\$module_v\""
  [ ${1%.sh} = $1 ] || eval "eval \"\$module_v\"" || echo "in module $1" >&2
  modules="$modules$newline$1"
  module_index=$((module_index + 1))
}
module_index=1
modules=boot.sh
meta_hook() meta_hooks="$meta_hooks$newline$(cat)"'
eval "$module_0"
module 'meta/struct.sh' <<'6e719a2f93ec39ed2c4fa7b2cddaf828a7906f88c1bd5c36fca81d4fab043893'
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
  str pr_s "$1"
  verb "$pr_s"
}
6e719a2f93ec39ed2c4fa7b2cddaf828a7906f88c1bd5c36fca81d4fab043893
module 'meta/ni-option.sh' <<'99cecd9d7da5e4adf23f0a7d82dccbba2af0c5932c22085ec6dc5cb6d4486c40'
# ni option data structure and generator

meta_hook <<'EOF'
defstruct option long short syntax fn description
hashmap long_options
hashmap short_options

defstruct option_syntax name mode accept description
hashmap option_syntaxes
EOF

option_str() eval "$1=\"-\$${2}_short|--\$${2}_long [\$${2}_syntax]" \
                  "\$${2}_fn(): \$${2}_description\""

# Defines a long and short option, with parsing hints. For example:
# defoption --group -g D group "groups rows by column value"
#
# This means the following:
# --group       long option name is "--group"; this is required
# -g            short option name is "-g"; can be '' for no short option
# D             option takes an optional number as an argument
# ni_group      compiler function to be called for the option
# "groups ..."  usage for the option
#
# If you then invoke ni as "ni -g4", "ni_group" will be called on "4" to
# interact with the compiler to generate the relevant shell script. See ni.sh
# for details about how this works.
defoption() {
  defoption_long="${1#--}"
  defoption_short="${2#-}"
  option defoption_new $defoption_long "$defoption_short" "$3" "$4" "$5"
  assoc $long_options $defoption_long $defoption_new
  [ -n "$defoption_short" ] \
    && assoc $short_options $defoption_short $defoption_new
}

# Defines an option argument syntax parser. The mode indicates when, and how
# much, the argument is accepted.
defoptionsyntax() {
  option_syntax defoptionsyntax_new "$@"
  assoc $option_syntaxes $1 $defoptionsyntax_new
}
99cecd9d7da5e4adf23f0a7d82dccbba2af0c5932c22085ec6dc5cb6d4486c40
module 'meta/list.sh' <<'08d0c5197e97ccfd5862e03d39effaaec2c7adf78d4845cca9000a2b76e42d5b'
# Linked list functions

list_reverse() {
  if [ -n "$2" ]; then
    uncons list_reverse_h list_reverse_t $2
    cons list_reverse_l $list_reverse_h $3
    list_reverse "$1" "$list_reverse_t" $list_reverse_l
  else
    eval "$1=\$3"
  fi
}

list_append() {
  if [ -n "$2" ]; then
    uncons list_append_h list_append_t $2
    set -- $1 "$list_append_h"
    list_append list_append_t "$list_append_t" "$3"
    cons $1 "$list_append_h" "$list_append_t"
  else
    eval "$1=\$3"
  fi
}

# Converts command-line arguments to a list
list() {
  list_r=$1
  list_cons=
  shift
  while [ $# -gt 0 ]; do
    cons list_cons "$1" "$list_cons"
    shift
  done
  list_reverse $list_r "$list_cons"
}

# Like the usual (apply), but doesn't accept any intermediate arguments; that
# is, it's just a function and a list.
apply_last() {
  apply_last_f=$1
  apply_last_r=$2
  apply_last_l=$3
  shift 3
  while [ -n "$apply_last_l" ]; do
    uncons apply_last_h apply_last_l $apply_last_l
    set -- "$@" "$apply_last_h"
  done
  $apply_last_f $apply_last_r "$@"
}
08d0c5197e97ccfd5862e03d39effaaec2c7adf78d4845cca9000a2b76e42d5b
module 'meta/vector.sh' <<'c70d72a04390dbb684d272167d9b9f2c9b50250bcb58232f45e473aec796177a'
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
c70d72a04390dbb684d272167d9b9f2c9b50250bcb58232f45e473aec796177a
module 'meta/cons.sh' <<'480b6319a67ca4786afa845e433dd50abde3ed865a08720d8fb507b27f3fb322'
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
480b6319a67ca4786afa845e433dd50abde3ed865a08720d8fb507b27f3fb322
module 'meta/hashmap.sh' <<'67846f18a425c510b0ac73ec8bf79a5c5c8c88a315f163e9498ab81895fa2f50'
# Hashmap data structure
# (Not technically a hashmap because we piggyback off of the parent shell's
#  global variable table; i.e. we're not doing anything clever here.)

meta_hook <<'EOF'
defmulti keys
defmulti get
defmulti contains
EOF

# Constructed from pairs of arguments:
# hashmap result k1 v1 k2 v2 ... kn vn
hashmap() {
  hashmap_r=$1
  cell hashmap_cell hashmap
  shift
  while [ $# -gt 0 ]; do
    assoc $hashmap_cell "$1" "$2"
    shift 2
  done
  eval "$hashmap_r=\$hashmap_cell"
}

hashmap_gc() {
  hashmap_gc_keys=
  keys hashmap_gc_l $2
  while [ -n "$hashmap_gc_l" ]; do
    uncons hashmap_gc_k hashmap_gc_l $hashmap_gc_l
    hashmap_gc_keys="$hashmap_gc_keys $hashmap_gc_k"
  done
  eval "$1=\"\$hashmap_gc_keys\""
}

hashmap_str() {
  hashmap_str_s=
  keys hashmap_str_l $2
  while [ -n "$hashmap_str_l" ]; do
    uncons hashmap_str_k hashmap_str_l $hashmap_str_l
    set -- "$1" "$2" "$hashmap_str_s $hashmap_str_k" "$hashmap_str_l"
    get hashmap_str_v $2 $hashmap_str_k
    str hashmap_str_v $hashmap_str_v
    hashmap_str_s="$3 $hashmap_str_v"
    hashmap_str_l="$4"
  done
  eval "$1=\"{\${hashmap_str_s# }}\""
}

assoc() eval "${1}_key_$2=\"\$3\"
              if [ -z \"\$${1}_cell_$2\" ]; then
                cons ${1}_keys \"\$2\" \$${1}_keys
                ${1}_cell_$2=\$${1}_keys
              fi"

hashmap_keys()     eval "$1=\$${2}_keys"
hashmap_get()      eval "$1=\"\$${2}_key_$3\""
hashmap_contains() eval "[ -n \"${2}_cell_$3\" ] && $1=t || $1="
67846f18a425c510b0ac73ec8bf79a5c5c8c88a315f163e9498ab81895fa2f50
module 'self/repl.sh' <<'ca80a505aa1b1a5c93c267861feea2bfc757b64c9d126478fc621950bc5677b9'
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
        | exec sh "$@") \
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
        exec "${SHELL:-bash}" "$@" || exec sh "$@") \
    && inhume "$repl_stateless_self_dir" \
    && rm -r "$repl_stateless_self_dir"
}
ca80a505aa1b1a5c93c267861feea2bfc757b64c9d126478fc621950bc5677b9
module 'self/fs.sh' <<'9ec1f481526cf3983cca1a6476d0ffe4d2c7da7b7d4c091a86435a050c806569'
# Exhume/inhume self to/from FS
# Usage: exhume existing-directory (populates self to directory)
exhume() {
  exhume_i=0
  exhume_old_ifs="$IFS"
  IFS="$newline"
  for exhume_m in $modules; do
    [ "${exhume_m%/*}" = "$exhume_m" ] || mkdir -p "$1/${exhume_m%/*}"
    eval "verb \"\$module_$exhume_i\"" > "$1/$exhume_m"
    exhume_i=$((exhume_i + 1))
  done
  IFS="$exhume_old_ifs"
}

# Usage: inhume exhumed-directory (populates directory to self)
# NB: just as exhume doesn't create one, this function doesn't remove the
# directory. Also, inhumed files are in arbitrary order except for boot.sh,
# which always goes first.
inhume() {
  module_index=0
  module boot.sh "$(cat "$1/boot.sh")"
  inhume_old_ifs="$IFS"
  inhume_dir="${1%/}"

  # Always inhume meta stuff first
  IFS="$newline"
  for inhume_f in $(find "$inhume_dir/meta" -type f); do
    IFS="$inhume_old_ifs"
    module "${inhume_f#$inhume_dir/}" "$(cat "$inhume_f")"
  done

  IFS="$newline"
  for inhume_f in $(find "$inhume_dir" -type f); do
    IFS="$inhume_old_ifs"
    [ "$inhume_f" != "$inhume_dir/boot.sh" ] \
      && [ "${inhume_f#$inhume_dir/meta/}" = "$inhume_f" ] \
      && module "${inhume_f#$inhume_dir/}" "$(cat "$inhume_f")"
  done
  IFS="$inhume_old_ifs"
}

# Writes the current image to the specified file. If the resulting image fails,
# prints the name of the tempfile where it is stored.
save() {
  save_file=$(self | canonical_file)
  save_state=$(self | sha3)
  save_check=$(sh "$save_file" --internal-state)
  if [ "$save_state" = "$save_check" ]; then
    chmod 755 "$save_file"
    mv "$save_file" "$1"
  else
    err "ni: save failed; $save_state " \
        "    != $save_check" \
        "    (this means the new object failed to convince this one that " \
        "     it had managed to store everything and operate correctly)"
    save_fail_file="${TMPDIR:-/tmp}/${save_file#$self_tmpdir/}"
    mv "$save_file" "$save_fail_file"
    verb "$save_fail_file"
    return 1
  fi
}
9ec1f481526cf3983cca1a6476d0ffe4d2c7da7b7d4c091a86435a050c806569
module 'self/image.sh' <<'1806392814e051c2e785915d3aec1e3b09247e9346d65e4fea51e1ae9f60233d'
# Retrieves a module's text by name
# Usage: module_get destination_var module/name
# Does nothing if the variable is already set, which makes it possible to use
# this function repeatedly without a performance hit.
module_get() {
  eval "[ -n \"\$$1\" ]" && return
  module_get_old_ifs="$IFS"
  IFS="$newline"
  module_get_i=0
  for module_get_name in $modules; do
    if [ "$2" = "$module_get_name" ]; then
      eval "$1=\"\$module_$module_get_i\""
      IFS="$module_get_old_ifs"
      return 0
    fi
    module_get_i=$((module_get_i + 1))
  done
  IFS="$module_get_old_ifs"
}

# Prints a representation of this object to stdout. If invoked with --no-main
# as the first option, no call to main() will be generated at the end of the
# image. If invoked with --no-boot, boot.sh will be serialized as a normal
# module rather than treated specially (which will produce a broken image, but
# may be useful for serialization purposes).
self() {
  self_main='main "$@"'
  self_boot=t
  while [ $# -gt 0 ]; do
    [ "$1" = "--no-main" ] && self_main=
    [ "$1" = "--no-boot" ] && self_boot=
    shift
  done

  main_setup
  [ -n "$self_boot" ] \
    && verb "#!/bin/sh" \
            "# <body style='display:none'><script type='ni' id='self'>" \
            "# Self-modifying ni image: https://github.com/spencertipping/ni" \
            "module_0='$module_0'" \
            'eval "$module_0"'

  self_old_ifs="$IFS"
  IFS="$newline"
  self_i=0
  for self_m in $modules; do
    if [ -z "$self_boot" ] || [ "$self_m" != boot.sh ]; then
      eval "self_marker=\"\$(verb \"\$module_$self_i\" | exec "$sha3")\""
      verb "module '$self_m' <<'$self_marker'"
      eval "verb \"\$module_$self_i\""
      verb "$self_marker"
    fi
    self_i=$((self_i + 1))
  done
  IFS="$self_old_ifs"
  verb "# </""script>" "$self_main"
}
1806392814e051c2e785915d3aec1e3b09247e9346d65e4fea51e1ae9f60233d
module 'ni/ni.sh' <<'b0d27ef741a3b327e0f55b936ceeda3d2ce11a01edc792503849f338299f51d2'
# ni frontend core definitions: syntactic structures and multimethods
# See also meta/ni-option.sh for the metaprogramming used by home/conf.

meta_hook <<'EOF'
defstruct quasifile name

# Complex commands
defstruct lambda body           # [ ... ] or ^x
defstruct lambdafork body       # @[ ... ] or @^x
defstruct lambdaplus body       # -[ ... ]
defstruct lambdamix body        # -@[ ... ]
defstruct branch map            # { x ... , y ... , ... }
defstruct branchfork map        # @{ x ... , y ... , ... }
defstruct branchsub map         # -{ ... }
defstruct branchmix map         # -@{ ... }

# Pipeline compilation multimethods
defmulti compile                # compile to a shell command
defmulti describe               # plain-English description
EOF

# Option parsing
# Usage: ni_parse destination_var $vector_ref
#
# $vector_ref comes from using "vector" to convert "$@" to a vector object.
# ni_parse will then consume the vector, which amounts to shifting it until
# it's empty.
ni_parse() {
  cons ni_parse_stack   '' ''
  cons ni_parse_command '' ''
  ni_parse_r="$1"
  ni_parse_v="$2"

  while lpop ni_parse_option "$ni_parse_v"; do
    ni_parse_closers=0
    ni_parse_push_one=t
    ni_parse_to_shift=1

    # First handle syntax cases.
    case "$ni_parse_option" in
    '[')   cons ni_parse_command lambda     $ni_parse_command ;;
    '@[')  cons ni_parse_command lambdafork $ni_parse_command ;;
    '-[')  cons ni_parse_command lambdaplus $ni_parse_command ;;
    '-@[') cons ni_parse_command lambdamix  $ni_parse_command ;;
    '{')   cons ni_parse_command branch     $ni_parse_command ;;
    '@{')  cons ni_parse_command branchfork $ni_parse_command ;;
    '-{')  cons ni_parse_command branchsub  $ni_parse_command ;;
    '-@{') cons ni_parse_command branchmix  $ni_parse_command ;;

    ']'|'}') ni_parse_push_one= ni_parse_closers=1 ;;

    # Now handle general options and quasifiles.
    *)
      ni_parse_push_one=

      # Look for any lambda prefixes, forking or not. These end as soon as
      # we've consumed all arguments for the quoted operator, so we increment
      # ni_parse_closers for each prefix.
      while [ "x${ni_parse_option#^}"  != "x$ni_parse_option" ] \
         || [ "x${ni_parse_option#@^}" != "x$ni_parse_option" ]; do
        cons ni_parse_stack '' $ni_parse_stack
        ni_parse_closers=$((ni_parse_closers + 1))
        if [ "x${ni_parse_option#@}" != "x$ni_parse_option" ]; then
          # Forking short lambda
          cons ni_parse_command lambdafork $ni_parse_command
        else
          # Non-forking short lambda
          cons ni_parse_command lambda $ni_parse_command
        fi
      done

      if [ "x${ni_parse_option#--}" != "x$ni_parse_option" ]; then
        # Long option parsing mode: retrieve the option's syntax and parse
        # additional arguments, stuffing them all into the specified option
        # structure.
        get ni_parse_option_ref $long_options "${ni_parse_option#--}"
        if [ -z "$ni_parse_option_ref" ]; then
          err "ni: unknown long option $ni_parse_option"
          return 1
        fi

        syntax ni_parse_syntax "$ni_parse_option_ref"
        fn     ni_parse_fn     "$ni_parse_option_ref"

        ni_parse_arguments=

        # After a long option, all arguments are expected to be provided as
        # separate options; i.e. we don't have any shorthands for
        # option-packing, since clearly the user cares nothing about concision.
        while [ -n "$ni_parse_syntax" ]; do
          substr ni_parse_syntax_x "$ni_parse_syntax" 0 1
          ni_parse_syntax="${ni_parse_syntax#?}"

          get ni_parse_optionsyntax "$option_syntaxes" "$ni_parse_syntax_x"
          mode ni_parse_optionmode $ni_parse_optionsyntax

          # TODO: can this work with lambdas? Is it sufficient to do a naive
          # forward-traversal to see where the lambda ends? The only problem is
          # that lambda delimiters are arguably subject to interpretation: we
          # don't know whether one is literal, or whether it's an argument to
          # an option that expects it.
          case "$ni_parse_syntax_x" in
          s|v) shift; cons ni_parse_arguments "$1" "$ni_parse_arguments" ;;
          D)   if string_matches "$2" "[0-9]"; then
                 shift
                 cons ni_parse_arguments "$1" "$ni_parse_arguments"
               fi ;;
          F)   if string_matches "$2" "[-.0-9]"; then
                 shift
                 cons ni_parse_arguments "$1" "$ni_parse_arguments"
               fi
          esac
        done
      elif [ "x${ni_parse_option#-}" != "x$ni_parse_option" ]; then
        TODO short option $ni_parse_option
      else
        TODO quasifile $ni_parse_option
      fi
    esac

    [ "$ni_parse_push_one" = t ] && cons ni_parse_stack '' $ni_parse_stack

    while [ $ni_parse_closers -gt 0 ]; do
      uncons ni_parse_the_cmd ni_parse_command $ni_parse_command
      uncons ni_parse_this    ni_parse_stack   $ni_parse_stack
      list_reverse ni_parse_this "$ni_parse_this"

      # TODO: no longer accurate; we need to apply a constructor here.
      vector ni_parse_v "$ni_parse_the_cmd" "$ni_parse_this"

      eval "cons \${ni_parse_stack}_h \$ni_parse_v \$${ni_parse_stack}_h"
      ni_parse_closers=$((ni_parse_closers - 1))
    done

    shift $ni_parse_to_shift
  done

  uncons "$ni_parse_r" ni_parse_stack "$ni_parse_stack"
  if ! [ -z "$ni_parse_stack" ]; then
    err "ni: unclosed bracket(s) or brace(s)"
    return 1
  fi
}

# Compiles a structure produced by ni_parse, returning the jit context to
# execute it. Usage:
# ni_compile dest_var $cons_structure
ni_compile() {
  TODO ni_compile
}
b0d27ef741a3b327e0f55b936ceeda3d2ce11a01edc792503849f338299f51d2
module 'ni/quasifile.sh' <<'2bfae0625668105102b2929fa3a85413c76dba3fd7e58f3bf0e9eaf4e3311f91'
# Quasifile object representation
# TODO
:
2bfae0625668105102b2929fa3a85413c76dba3fd7e58f3bf0e9eaf4e3311f91
module 'ni/sorting.sh' <<'337e7d047c2cfd0609776a5111d8e1f450ded5ae0a58be4553c02400a4ed6cb1'
# Sorting operators and function definitions
meta_hook <<'EOF'
defstruct ni_group column_spec ordering
defstruct ni_order column_spec ordering
EOF

ni_group() {
  TODO ni_group
}
337e7d047c2cfd0609776a5111d8e1f450ded5ae0a58be4553c02400a4ed6cb1
module 'bin/sha3.c' <<'b04f3235cf9c75f6ee101abf07699975a65413c33078c14cd24acb46229b60f1'
/* Calculates the SHA3-256 of stdin and argv. This is used throughout ni to
 * cache intermediate results.
 *
 * Code from https://github.com/gvanas/KeccakCodePackage/blob/master/Standalone/CompactFIPS202/Keccak-more-compact.c;
 * modified here to read from a file descriptor and to alias pointers for a
 * speedup of about 4x */

#include <unistd.h>
#include <sys/types.h>

#define FOR(i,n) for(i=0; i<n; ++i)
typedef unsigned char u8;
typedef unsigned long long int u64;
typedef unsigned int ui;

void Keccak(ui r, ui c, const u8 *in, u64 inLen, u8 sfx, u8 *out, u64 outLen);

int LFSR86540(u8 *R) { (*R)=((*R)<<1)^(((*R)&0x80)?0x71:0); return ((*R)&2)>>1; }
#define ROL(a,o) ((((u64)a)<<o)^(((u64)a)>>(64-o)))
#define rL(x,y) (*(u64*)((u8*)s+8*(x+5*y)))
#define wL(x,y,l) (*(u64*)((u8*)s+8*(x+5*y))=(l))
#define XL(x,y,l) (*(u64*)((u8*)s+8*(x+5*y))^=(l))
void KeccakF1600(void *s)
{
    ui r,x,y,i,j,Y; u8 R=0x01; u64 C[5],D;
    for(i=0; i<24; i++) {
        /*θ*/ FOR(x,5) C[x]=rL(x,0)^rL(x,1)^rL(x,2)^rL(x,3)^rL(x,4); FOR(x,5) { D=C[(x+4)%5]^ROL(C[(x+1)%5],1); FOR(y,5) XL(x,y,D); }
        /*ρπ*/ x=1; y=r=0; D=rL(x,y); FOR(j,24) { r+=j+1; Y=(2*x+3*y)%5; x=y; y=Y; C[0]=rL(x,y); wL(x,y,ROL(D,r%64)); D=C[0]; }
        /*χ*/ FOR(y,5) { FOR(x,5) C[x]=rL(x,y); FOR(x,5) wL(x,y,C[x]^((~C[(x+1)%5])&C[(x+2)%5])); }
        /*ι*/ FOR(j,7) if (LFSR86540(&R)) XL(0,0,(u64)1<<((1<<j)-1));
    }
}

#define IN_BLOCK 4

int main()
{
    /*initialize*/ u8 eof = 0, ia[1088 * IN_BLOCK], *in = ia, oa[32], *out = oa; u64 r, inLen = 0; u8 s[200]; ui R=1088/8; ui i,b=0; u64 outLen=32; FOR(i,200) s[i]=0;
    /*absorb*/ while (!eof) {r = 0;
                             while ((inLen += r) < 1088 * IN_BLOCK && !eof) eof |= !(r = read(0, (in = ia) + inLen, sizeof(ia) - inLen));
                             while(inLen>0) { b=(inLen<R)?inLen:R; FOR(i,b) s[i]^=in[i]; in+=b; inLen-=b; if (b==R) { KeccakF1600(s); b=0; } } }
    /*pad*/ s[b]^=6; if((6&0x80)&&(b==(R-1))) KeccakF1600(s); s[R-1]^=0x80; KeccakF1600(s);
    /*squeeze*/ while(outLen>0) { b=(outLen<R)?outLen:R; FOR(i,b) out[i]=s[i]; out+=b; outLen-=b; if(outLen>0) KeccakF1600(s); }

    u8 oh[65], *hex="0123456789abcdef"; FOR(i, 32) { oh[i<<1]=hex[oa[i]>>4]; oh[i<<1|1]=hex[oa[i]&15]; }
    oh[64]='\n'; write(1, oh, 65); return 0;
}
b04f3235cf9c75f6ee101abf07699975a65413c33078c14cd24acb46229b60f1
module 'main.sh' <<'9237018979937a8fe909aedb3feb1cfc59429b3b1c01f643fc31223ffdfa935a'
# Main function, called automatically with all command-line arguments. The call
# is hard-coded in the image generator, so main() is a magic name.

main_is_setup=
main_setup() {
  [ -n "$main_is_setup" ] && return
  eval "$meta_hooks"
  eval "$setup_hooks"
  main_is_setup=t
}

main() {
  main_setup

  module_get conf_module home/conf
  eval "$conf_module"

  # Handle image-level special options
  case "$1" in
  --edit)
    shift
    err "ni: entering a shell to edit the current image" \
        "    any changes you make to files here will be reflected in ni" \
        "    and written back into $0 (assuming the result is able to" \
        "    function correctly; otherwise a tempfile will be saved)" \
        "" \
        "    exit the shell to save your changes and return" \
        ""
    repl_stateless "$@"
    save "$0"
    ;;

  --install)
    shift
    main_save_dest="${1:-$HOME/bin/ni}"
    [ "${main_save_dest#/}" = "$main_save_dest" ] \
      && main_save_dest="$PWD/$main_save_dest"
    main_save_dir="${main_save_dest%/*}"
    main_save_i=0
    mkdir -p "$main_save_dir"
    if [ -e "$main_save_dest" ]; then
      while [ -e "$main_save_dest$main_save_i" ]; do
        main_save_i=$((main_save_i + 1))
      done
      save "$main_save_dest$main_save_i" || return $?
    else
      save "$main_save_dest" || return $?
      [ $# -gt 0 ] || err "ni: installed image at $main_save_dest"
    fi
    ;;

  --init)
    if [ -e ni ]; then
      err "ni: image already exists here; not overwriting"
    else
      save ni && err "ni: created image at ./ni" \
              || return $?
    fi
    ;;

  # FS interop
  --unpack|--expand|--exhume) shift; mkdir "$1" && exhume "$1" ;;
  --use|--intern|--inhume)    shift; inhume "$1" && save "$0" ;;

  # Internal options for the build process and debugging
  --internal-repl)  shift; repl_sh "$@" ;;
  --internal-self)  shift; self "$@" ;;
  --internal-sha3)  shift; sha3 ;;
  --internal-state) shift; self "$@" | exec "$sha3" ;;

  *)
    vector main_options "$@"
    str s1 $main_options
    verb "options = $s1"
    ni_parse main_parsed "$main_options"
    str s2 $main_parsed
    verb "parsed  = $s2"
    ;;
  esac

  eval "$shutdown_hooks"
}
9237018979937a8fe909aedb3feb1cfc59429b3b1c01f643fc31223ffdfa935a
module 'jit/jit-sh.sh' <<'c9ddfdf2b372f6338580bd876b4a3cf06ee1212b0595f4b8de4c015a83f788a9'
# JIT support for POSIX shell programs
#
# jit_program=$(jit_sh <<'EOF'
# echo "hello world $*"
# EOF
# )
#
# $jit_program "$@"             # to execute the program
# jit_sh_free $jit_program      # to deallocate the program

jit_sh() {
  tmpdir
  jit_sh_index=$((jit_sh_index + 1))
  jit_sh_source="$self_tmpdir/jit-sh-$jit_sh_index"
  { echo "#!/bin/sh"; cat; } > "$jit_sh_source"
  chmod 700 "$jit_sh_source"
  verb "$jit_sh_source"
}

jit_sh_free() tmpdir_rm "$1"
c9ddfdf2b372f6338580bd876b4a3cf06ee1212b0595f4b8de4c015a83f788a9
module 'jit/jit-c.sh' <<'d2265f53d8d9290885422cd2dd5e5ef2f3e69bfea4ab6d139acc6fbeac24bed1'
# JIT support for C99 programs
# Calling convention is like this, except that heredocs inside $() don't seem
# to be allowed:
#
# jit_program=$(jit_c [c99-option...] <<'EOF'
# #include <stdio.h>
# int main() {
#   printf("hello world\n");
#   return 0;
# }
# EOF
# )
#
# $jit_program "$@"             # to execute the program
# jit_c_free $jit_program       # to deallocate the program

# NB: this version is used for bootstrapping; specifically, we need to compile
# the sha3 program before we'll have any support for unified jit.
jit_c_base() {
  tmpfile jit_c_base_source .c
  cat > "$jit_c_base_source"
  c99 "$@" "$jit_c_base_source" -o "${jit_c_base_source%.c}"
  verb "${jit_c_base_source%.c}"
}

# This version is what you would normally use; it will reuse existing programs
# rather than recompiling them.
jit_c() {
  jit_c_source="$(canonical_file .c)"
  [ -x "${jit_c_source%.c}" ] \
    || c99 "$@" "$jit_c_source" -o "${jit_c_source%.c}"
  verb "${jit_c_source%.c}"
}

jit_c_free() tmpdir_rm "$1" "$1.c"
d2265f53d8d9290885422cd2dd5e5ef2f3e69bfea4ab6d139acc6fbeac24bed1
module 'jit/jit-mvn.sh' <<'56d5b4c2e782305b4a824587cbecb0b18e42c98f3295bab87f1c7367ef81ae5b'
# JVM JIT using maven as the frontend

# POM generation stuff
# The goal here is to make it trivial to build a working POM file, without
# typing out a whole bunch of XML. This is done by defining POM-macro functions
# that handle various command-line options and generate the relevant XML
# fragments for you. For example, here's the dependency generator:

mvn_pom_deps() {
  verb '<dependencies>'
  for mvn_pom_deps_d; do
    # Dependencies are specified using this syntax:
    # [groupId/]artifactId=version[:scope]
    mvn_pom_deps_ga="${1%%=*}"
    mvn_pom_deps_vs="${1##*=}"
    mvn_pom_deps_g="${mvn_pom_deps_ga%%/*}"
    mvn_pom_deps_s="${mvn_pom_deps_vs##*:}"
    [ "$mvn_pom_deps_s" = "$mvn_pom_deps_vs" ] \
      && mvn_pom_deps_s= \
      || mvn_pom_deps_s="<scope>$mvn_pom_deps_s</scope>"
    verb "<dependency>" \
         "<groupId>$mvn_pom_deps_g</groupId>" \
         "<artifactId>${mvn_pom_deps_ga##*/}</artifactId>" \
         "<version>${mvn_pom_deps_vs%%:*}</version>" \
         "$mvn_pom_deps_s</dependency>"
  done
  verb '</dependencies>'
}

# Calling syntax for the above would be this:
#
# mvn_pom --deps foo.bar/bif=1.0.0-SNAPSHOT:development \
#                foo.bar/baz=10:test \
#         --...
#
# ni detects the end of arguments to a generator by looking for the next option
# starting with --.

mvn_pom() {
  : TODO
}

# Usage: jit_program=$(jit_mvn Foobar [-Dgroup/artifact=version ...] ... <<'EOF'
# public class Foobar {
#   public static void main(String[] args) {
#     System.out.println("hello world!");
#   }
# }
# EOF
# )
#
# $jit_program "$@"
# jit_mvn_free $jit_program
jit_mvn() {
  jit_mvn_result=$1
  jit_mvn_main=$2
}
56d5b4c2e782305b4a824587cbecb0b18e42c98f3295bab87f1c7367ef81ae5b
module 'home/conf' <<'ffb9884766d89f44d9807cc6bf02544a862e51d1245ded5102eb28aba9736c08'
# ni configuration, including CLI option mapping. Uses generators defined in
# meta/ni-option.sh.
#
# Valid argument-parsing syntax specifiers are:
#
#   s   string: rest of short argument, or next whole argument, or lambda
#   v   varstring: one char of short, next whole, or lambda
#   D   as many digits as we have in short mode, next if digits in long mode
#   F   like D, but includes . - (mnemonic float)
#   R   like F, but includes : , (mnemonic range)
#
# D, F, and R are uppercase because they all indicate optional quantities.

# Sorting operations
defoption --group  -g D ni_group
defoption --order  -o D ni_order
defoption --rorder -O D ni_rorder
ffb9884766d89f44d9807cc6bf02544a862e51d1245ded5102eb28aba9736c08
module 'util/tmpfile.sh' <<'716d8ab227a740cccb7bc4a9ce8b7a4cb5f3e7b1c7b6bbaf694fe05acc605b48'
# Creates uniquely-named files in the temporary directory

# Usage: tmpdir destination_var
# Returns the original tmpdir if the destination var is already populated and
# exists.
tmpdir() {
  eval "[ -e \"\$${1:-self_tmpdir}/.this-is-a-ni-tmpdir\" ]" && return
  tmpdir_prefix="${TMPDIR:-/tmp}/ni-$$"
  tmpdir_index=0
  until mkdir "$tmpdir_prefix-$tmpdir_index" 2>/dev/null; do
    tmpdir_index=$((tmpdir_index + 1))
  done
  touch "$tmpdir_prefix-$tmpdir_index/.this-is-a-ni-tmpdir"
  eval "${1:-self_tmpdir}=\"\$tmpdir_prefix-\$tmpdir_index\""
}

tmpdir_free() {
  [ $# -eq 0 ] && set -- "$self_tmpdir"
  if [ ! -d "$1" ]; then
    return 0
  elif [ ! -e "$1/.this-is-a-ni-tmpdir" ]; then
    err "ni: trying to clean up a tmpdir \"$1\"" \
        "    but this tmpdir doesn't appear to have been created by ni" \
        "    in this case, not doing anything"
    return 1
  fi
  rm -r "$1"
}

# Removes files, but makes sure they're in the temporary directory first. This
# is a sanity check to avoid blowing away random stuff on the filesystem, e.g.
# if a variable got cleared by mistake. Forwards the "-r" option if passed
# first.
tmpdir_rm() {
  tmpdir_rm_options=
  if [ "x$1" = "x-r" ]; then
    tmpdir_rm_options=-r
    shift
  fi
  for tmpdir_rm_f; do
    if [ -n "$self_tmpdir" ] \
       && [ "${tmpdir_rm_f#$self_tmpdir/}" != "$tmpdir_rm_f" ]; then
      rm $tmpdir_rm_options -- "$tmpdir_rm_f"
    else
      err "ni: attempting to remove \"$tmpdir_rm_f\", which is not" \
          "    located in a temp directory created by ni (in this case" \
          "    not doing anything)"
    fi
  done
}

# It's important to create a tmpdir at startup. If we don't, then a subprocess
# will; but subprocesses can't modify our variable-space so we won't see it
# (and therefore won't clean it up). The way around this is to prepend tmpdir
# to the list of start hooks so it happens before anything gets jitted.
setup_hooks="tmpdir$newline$start_hooks"
shutdown_hooks="$shutdown_hooks${newline}tmpdir_free"

# Usage: tmpfile dest_var [suffix]
tmpfile() {
  tmpdir
  tmpfile_i=0
  while [ -e "$self_tmpdir/$tmpfile_i$2" ]; do
    tmpfile_i=$((tmpfile_i + 1))
  done
  touch "$self_tmpdir/$tmpfile_i$2"
  eval "$1=\"\$self_tmpdir/\$tmpfile_i\$2\""
}

# Usage: dest_var=$(data-source | canonical_file [suffix])
# NB: uses disk-buffering, not memory-buffering. Does not, however, compress
# the file.
canonical_file() {
  tmpfile canonical_file_tmp $1
  cat > "$canonical_file_tmp"
  canonical_file_sha="$(sha3 < "$canonical_file_tmp")"
  mv "$canonical_file_tmp" "$self_tmpdir/$canonical_file_sha$1"
  verb "$self_tmpdir/$canonical_file_sha$1"
}

# Usage: data-source | file_buffer | ...
# Essentially a passthrough, but completely buffers the input data first. This
# works around a shell/pipe bug that otherwise causes truncated data streams.
file_buffer() {
  tmpfile file_buffer_tmp
  cat > "$file_buffer_tmp"
  cat "$file_buffer_tmp"
  tmpdir_rm "$file_buffer_tmp"
}
716d8ab227a740cccb7bc4a9ce8b7a4cb5f3e7b1c7b6bbaf694fe05acc605b48
module 'util/string.sh' <<'eb820f0e739dc6b0c2062c225c6ff883d80e248543dcaef1ba2bb344ea7032af'
# String functions

# Substring function
# POSIX shell doesn't have an expansion for this (bash has ${var:start:len}),
# so we need to make one. This isn't especially fast, but it does work. All
# indexes start at zero.
#
# Usage: substr dest_var s start [length]
substr() {
  substr_start=$3
  substr_mask=
  substr_double=?
  while [ $substr_start -gt 0 ]; do
    [ $((substr_start & 1)) -eq 1 ] && substr_mask="$substr_mask$substr_double"
    substr_double="$substr_double$substr_double"
    substr_start=$((substr_start >> 1))
  done

  if [ -n "$4" ]; then
    eval "$1=\"\$(printf %.$4s \"\${2#\$substr_mask}\")\""
  else
    eval "$1=\"\${2#\$substr_mask}\""
  fi

  # These could be quite large, so go ahead and free some memory
  substr_double=
  substr_mask=
}

# Returns the number of characters at the beginning of a string that match a
# specified pattern.
# Usage: matching_chars dest_var "$s" [0-9a-f]
matching_chars() {
  # TODO: n lg n, not n²
  matching_chars_n=0
  matching_chars_s="$2"
  while [ "x${matching_chars_s#$3}" != "x$matching_chars_s" ]; do
    matching_chars_s="${matching_chars_s#?}"
    matching_chars_n=$((matching_chars_n + 1))
  done
  eval "$1=\$matching_chars_n"
}

# Indicates with its exit code whether every character in the given string
# matches the specified pattern.
string_matches() {
  matching_chars string_matches_n "$1" "$2"
  [ ${string_matches_n} -eq "${#1}" ]
}
eb820f0e739dc6b0c2062c225c6ff883d80e248543dcaef1ba2bb344ea7032af
module 'util/verb.sh' <<'bc4a06e43573a26490686bf3153f0609e3d0a933e41d1b84c50b854cc676edfd'
# Safe echo: works around the POSIX statement that "echo" is allowed to
# interpret its arguments

# Interesting bug workaround due to data truncation:
# https://gist.github.com/stfactual/dd9ee48c964067453ff4
verb_1024_q=?
while [ ${#verb_1024_q} -lt 1024 ]; do
  verb_1024_q="$verb_1024_q$verb_1024_q"
done

verb() {
  for verb_arg; do
#    while [ ${#verb_arg} -gt 1024 ]; do
#      printf %.1024s "$verb_arg"
#      verb_arg="${verb_arg#$verb_1024_q}"
#    done
    printf "%s\n" "$verb_arg"
  done
}

err() verb "$@" >&2
bc4a06e43573a26490686bf3153f0609e3d0a933e41d1b84c50b854cc676edfd
module 'util/todo.sh' <<'c7e6b724df83e49ba42c45c1f6f3b0835043d85751778589f58e75dabf6c7a7c'
# Support for todo-functions in code
TODO() {
  err "todo: $*"
  return 1
}
c7e6b724df83e49ba42c45c1f6f3b0835043d85751778589f58e75dabf6c7a7c
module 'util/sha3.sh' <<'05375d44b20622b02a04fd9594890166c9eec2641e851f3d76caadfec8720289'
# Support for hashing arbitrary data through the jit-C interface
sha3() "$sha3"

sha3_setup() {
  [ -n "$sha3" ] && return
  module_get sha3_source bin/sha3.c
  sha3="$(verb "$sha3_source" | jit_c_base)"            # jit_c_base !!!
  unset sha3_source
}

setup_hooks="$setup_hooks${newline}sha3_setup"

# NB: no shutdown hook to free the jit context here, since the sha3 program
# should exist as long as ni maintains its tmpdir.
05375d44b20622b02a04fd9594890166c9eec2641e851f3d76caadfec8720289
# </script>
main "$@"

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
module 'meta/struct.sh' <<'d735fa546b3b8336222b3f2a82d286f4e8806954e6dabbac5bab2108ed33d9b4'
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
    eval "$defstruct_field() eval \"\$1=\\\"\\\$\${2}_$defstruct_field\"\\\""
    eval "${defstruct_field}_set() eval \"\${1}_$defstruct_field=\\\"\\\$2\\\"\""
    defstruct_i=$((defstruct_i + 1))
  done

  eval "$defstruct_ctor$newline\$1=\$${defstruct_name}_cell\"; }"
  eval "$defstruct_str>\\\"\""
  eval "$defstruct_visitor\\\"\""
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
d735fa546b3b8336222b3f2a82d286f4e8806954e6dabbac5bab2108ed33d9b4
module 'meta/ni-option.sh' <<'7eea35b6a521da042ad249230af04dba287e31305bba47b71986b589081a671a'
# ni option data structure and generator

meta_hook <<'EOF'
defstruct option long short syntax fn description
hashmap long_options
hashmap short_options
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
7eea35b6a521da042ad249230af04dba287e31305bba47b71986b589081a671a
module 'meta/list.sh' <<'fe793c0e5ea99832e216de8c6ef46295bfb60915e4da3994c6e74947acf13e17'
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
fe793c0e5ea99832e216de8c6ef46295bfb60915e4da3994c6e74947acf13e17
module 'meta/vector.sh' <<'a4ec29723e8afe0d87cbc735f9f837565c27c646d1391f13693351acc1502408'
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
  for vector_x; do
    eval "vector_n=\$${vector_cell}_n"
    eval "${vector_cell}_$vector_n=\"\$vector_x\"
          ${vector_cell}_n=\$(($vector_n + 1))"
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
a4ec29723e8afe0d87cbc735f9f837565c27c646d1391f13693351acc1502408
module 'meta/cons.sh' <<'383dfb475e9cdc3334055d883c1e64eb0e0b34501fa3907acfd7c77011a18f02'
# Cons cell primitive
meta_hook <<'EOF'
defstruct cons h t
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
383dfb475e9cdc3334055d883c1e64eb0e0b34501fa3907acfd7c77011a18f02
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
module 'self/repl.sh' <<'a5b5264bfe23eeb6a198958c9602bbc3bd917f3b3c2bb88fa4eba77e71add324'
# REPL environment for testing things and editing the image
# Explodes the image into the filesystem, cd's there, and runs a sub-shell
# that's prepopulated with all of ni's shell state. This means the subshell
# will be a POSIX shell, not bash, ksh, or csh.
#
# If you want your preferred shell in an exploded state directory (but without
# in-memory state), you can use repl_stateless.

repl_sh() {
  repl_sh_self_dir="$self_tmpdir/repl-sh-$(self | sha3)"
  repl_sh_state="$(self --no-main | jit_sh)"
  mkdir "$repl_sh_self_dir" \
    && exhume "$repl_sh_self_dir" \
    && (cd "$repl_sh_self_dir"; exec sh -i "$repl_sh_state") \
    && jit_sh_free "$repl_sh_state" \
    && inhume "$repl_sh_self_dir" \
    && tmpdir_rm -r "$repl_sh_self_dir"
}

repl_stateless() {
  repl_stateless_self_dir="$self_tmpdir/repl-stateless-$(self | sha3)"
  mkdir "$repl_stateless_self_dir" \
    && exhume "$repl_stateless_self_dir" \
    && (cd "$repl_stateless_self_dir"; exec "${SHELL:-bash}" || exec sh) \
    && inhume "$repl_stateless_self_dir" \
    && tmpdir_rm -r "$repl_stateless_self_dir"
}
a5b5264bfe23eeb6a198958c9602bbc3bd917f3b3c2bb88fa4eba77e71add324
module 'self/fs.sh' <<'a615a26b95db6aa5c312058c7a337d6584388004646fc5a5bc9795542826b312'
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
  save_check=$(sh "$save_file" --state)
  if [ "$save_state" = "$save_check" ]; then
    chmod 755 "$save_file"
    mv "$save_file" "$1"
  else
    err "ni: save failed; $save_state != $save_check"
    save_fail_file="${TMPDIR:-/tmp}/${save_file#$self_tmpdir/}"
    mv "$save_file" "$save_fail_file"
    verb "$save_fail_file"
    return 1
  fi
}
a615a26b95db6aa5c312058c7a337d6584388004646fc5a5bc9795542826b312
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
module 'ni/ni.sh' <<'0f6a73c7e5466515321c5233e8fedf7611090b9a63e08af8968d1a24f48a0d66'
# ni frontend: main argument parser and shell script compiler
# See also meta/ni-option.sh for the metaprogramming used by conf.sh.

# Takes command-line options exactly as provided, and compiles a shell script
# to execute the result. The resulting shell script may refer to other
# generated programs, so it isn't network-portable. (ni as a whole, however,
# is.)
ni_compile() {
  TODO ni_compile
}

# Parses command-line options and returns (as $$1) a cons tree containing their
# structural representation. Each cons element is a vector of [fn args...].
# There are some special commands:
#
# [lambda (...)]                # [ -x ... ] or ^x
# [lambdafork (...)]            # @[ -x ... ] or @^x
# [lambdaplus (...)]            # -[ ... ]
# [lambdamix (...)]             # -@[ ... ]
#
# [branch {...}]                # { x ... , y ... , ... }
# [branchfork {...}]            # @{ x ... , y ... , ... }
# [branchsub {...}]             # -{ ... }
# [branchmix {...}]             # -@{ ... }
#
# Names of quasifiles are wrapped in string references

ni_parse_options() {
  ni_parse_options_r="$1"
  eval "$1="
  shift

  TODO ni_parse_options
}
0f6a73c7e5466515321c5233e8fedf7611090b9a63e08af8968d1a24f48a0d66
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
module 'main.sh' <<'820b05982c7f7c49bb9a9bd2679563668375694e5d975e2f841d570c91bb3fda'
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
        "    function correctly; otherwise a tempfile will be saved)"
    repl_stateless "$@"
    main_save_result="$(save "$0")"
    if [ -n "$main_save_result" ]; then
      err "ni: the current image failed to save itself: preserving state;" \
          "    the broken image is saved as $main_save_result."
    fi
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

  --repl)  shift; repl_sh ;;
  --self)  shift; self "$@" ;;
  --sha3)  shift; sha3 ;;
  --state) shift; self "$@" | exec "$sha3" ;;

  *)
    ni_compile main_ni "$@" && "$main_ni"
    ;;
  esac

  eval "$shutdown_hooks"
}
820b05982c7f7c49bb9a9bd2679563668375694e5d975e2f841d570c91bb3fda
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
module 'home/conf' <<'a26233e58002cee486637e5a49d5565823523596c1a7b8495c70d4b4e802eeac'
# ni configuration, including CLI option mapping. Uses generators defined in
# meta/ni-option.sh.

# Sorting operations
defoption --group  -g D ni_group
defoption --order  -o D ni_order
defoption --rorder -O D ni_rorder
a26233e58002cee486637e5a49d5565823523596c1a7b8495c70d4b4e802eeac
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
module 'util/todo.sh' <<'eeb448fda0a8ccb41c9c7f26098fccf946e9d5e3b48818e1586fb82df9e0653f'
# Support for todo-functions in code
TODO() {
  err "todo: $@"
  return 1
}
eeb448fda0a8ccb41c9c7f26098fccf946e9d5e3b48818e1586fb82df9e0653f
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

# Hashmap data structure
# (Not technically a hashmap because we piggyback off of the parent shell's
#  global variable table; i.e. we're not doing anything clever here.)

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
  eval "$1=\"{\$hashmap_str_s}\""
}

assoc() eval "${1}_key_$2=\"\$3\"
              if [ -n \"\$${1}_cell_$2\" ]; then
                cons ${1}_keys \"\$2\" \$${1}_keys
                ${1}_cell_$2=\$${1}_keys
              fi"

keys()     eval "$1=\$${2}_keys"
get()      eval "$1=\"\$${2}_key_$3\""
contains() eval "[ -n \"${2}_cell_$3\" ] && $1=t || $1="

# Hashmap data structure
# (Not technically a hashmap because we piggyback off of the parent shell's 

# Constructed from pairs of arguments:
# hashmap result k1 v1 k2 v2 ... kn vn
hashmap() {
  hashmap_r=$1
  cell hashmap_cell hashmap
  hashmap_keylist=
  shift
  while [ $# -gt 0 ]; do
    eval "${hashmap_cell}_key_$1=\"\$2\""
    cons hashmap_keylist "$1" $hashmap_keylist
    eval "${hashmap_cell}_cell_$1=\"\$hashmap_keylist\""
    shift 2
  done
  eval "${hashmap_cell}_keys=\$hashmap_keylist
        $hashmap_r=\$hashmap_cell"
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

assoc() {
  : TODO
}

dissoc() {
  : TODO
}

get()      eval "$1=\"\$${2}_key_$3\""
contains() eval "[ -n \"${2}_cell_$3\" ] && $1=t || $1="

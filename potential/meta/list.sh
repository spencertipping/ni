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

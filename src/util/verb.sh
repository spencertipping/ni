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

#!/bin/bash
# Builds the core ni image from the files in src/.
cd $(dirname $0)

cat src/boot \
    src/lib/quote \
    src/lib/printer \
  > ni0

perl ni0 //ni > ni && cp ni ni0
perl ni0 //ni > ni
wc -c ni

if [[ "$(<ni0)" != "$(<ni)" ]]; then
  echo 'ni is unstable under replication'
  diff -C 3 ni0 ni
  exit 1
fi

chmod 755 ni

#!/bin/bash
# Builds the core ni image from the files in core/boot.
cd "$(dirname "$0")"

# Resource format is "<nlines> <filename>\n<data...>", e.g.
#
# 5 foo.pl
# #!/usr/bin/env perl
# # stuff
# while (<>) {
#   print "hi $_";
# }
#
# See src/ni for the logic that parses this from the __DATA__ section.

# NB: these three functions are named to correspond to directives in
# src/ni.map.
bootcode() { cat core/boot/ni; }

resource() {
  for r; do
    wc -l "$r"
    cat "$r"
  done
}

# Build the ni image by including the header verbatim, then bundling the rest
# of the files as resources. The header knows how to unpack resources from the
# __DATA__ section of the script, and it evaluates the ones ending in .pl. This
# mechanism makes it possible for ni to serialize its code without being stored
# anywhere (which is useful if you're piping it to a system whose filesystem is
# read-only).
. core/boot/ni.map > ni

chmod +x ni

wc -c ni

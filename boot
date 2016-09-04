#!/bin/bash
# Builds the core ni image from the files in src/.
cd $(dirname $0)

# Preprocessor to erase SDoc documentation. This minimizes the image size but
# still makes it possible to document code in some detail.
unsdoc() { perl -e 'print join "", grep !/^\h*[|A-Z]/ + s/^\h*c\n//,
                                   split /\n(\h*\n)+/, join "", <>'; }

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
# src/ni.map.sdoc.
bootcode() { cat src/ni; }

resource() {
  cd src
  for r; do
    wc -l $r
    cat $r
  done
  cd ..
}

lib() {
  for l; do
    resource $l/lib
    for f in $(< src/$l/lib); do
      resource $l/$f
    done
  done
}

# Build the ni image by including the header verbatim, then bundling the rest
# of the files as resources. The header knows how to unpack resources from the
# __DATA__ section of the script, and it evaluates the ones ending in .pl. This
# mechanism makes it possible for ni to serialize its code without being stored
# anywhere (which is useful if you're piping it to a system whose filesystem is
# read-only).
eval "$(unsdoc < src/ni.map.sdoc)" > ni

chmod +x ni

wc -c ni

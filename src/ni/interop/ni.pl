#!/usr/bin/env perl
# A standalone perl program that takes a row-mapper as a command-line argument.
# Some templates here are expanded by ni in response to variables about
# packages to include, version, etc.

package ni;

#USE_PACKAGES
#LIBRARIES

BEGIN {
  for my $i (0 .. 99) {
    eval "sub ni::f$i() {\$fields[$i]}";
    eval "sub ni::s$i() {\$fields[$i]}";
    eval "sub ni::i$i() {0 | \$fields[$i]}";
    eval "sub ni::d$i() {0 + \$fields[$i]}";
  }
}

our @fields;
sub fs {@fields}
sub r  {s/\n//g for @_; print join("\t", @_), "\n"}

my $compiled = eval "sub {$ARGV[0]\n}" // die "$ARGV[0]: $@";
chomp, &$compiled(@fields = split /\t/) while <STDIN>;

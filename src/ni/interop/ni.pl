#!/usr/bin/env perl
# A standalone perl program that takes a row-mapper as a command-line argument.
# Some templates here are expanded by ni in response to variables about
# packages to include, version, etc.

package ni;

#USE_PACKAGES
#LIBRARIES

sub r { join "\t", @_ }

my $compiled = eval "sub {$ARGV[0]\n}" // die "$ARGV[0]: $@";
while (<STDIN>) {
  /\n$/ ? print : print $_, "\n" for $compiled->(split /\t/);
}

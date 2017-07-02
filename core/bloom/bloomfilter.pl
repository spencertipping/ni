# Bloom filter library.
# A simple pure-Perl implementation of Bloom filters.

use Digest::MD5 qw/md5/;

# Swiped from https://hur.st/bloomfilter
sub bloom_args($$) {
  my ($n, $p) = @_;
  my $m = int 1 + $n * -log($p) / log(2 ** log 2);
  my $k = int 0.5 + log(2) * $m / $n;
  ($m, $k);
}

sub bloom_new($$) {
  my ($m, $k) = @_;
  ($m, $k) = bloom_args($m, $k) if $k < 1;
  pack("NN", $m, $k) . "\0" x ($m + 7 >> 3);
}

sub multihash($$) {
  my @hs;
  push @hs, unpack "N4", md5 $_[0] . scalar @hs until @hs >= $_[1];
  @hs[0..$_[1]-1];
}

# Destructively adds an element to the filter and returns the filter.
sub bloom_add($$) {
  my ($m, $k) = unpack "NN", $_[0];
  vec($_[0], $_ % $m + 64, 1) = 1 for multihash $_[1], $k;
  $_[0];
}

sub bloom_contains($$) {
  my ($m, $k) = unpack "NN", $_[0];
  vec($_[0], $_ % $m + 64, 1) || return 0 for multihash $_[1], $k;
  1;
}

# Set operations
sub bloom_intersect {
  local $_;
  my ($n, $k, $filter) = unpack "NNa*", shift;
  for (@_) {
    my ($rn, $rk) = unpack "NN", $_;
    die "cannot intersect two bloomfilters of differing parameters "
      . "($n, $k) vs ($rn, $rk)"
      unless $n == $rn && $k == $rk;
    $filter &= unpack "x8 a*", $_;
  }
  pack("NN", $n, $k) . $filter;
}

sub bloom_union {
  local $_;
  my ($n, $k, $filter) = unpack "NNa*", shift;
  for (@_) {
    my ($rn, $rk) = unpack "NN", $_;
    die "cannot union two bloomfilters of differing parameters "
      . "($n, $k) vs ($rn, $rk)"
      unless $n == $rn && $k == $rk;
    $filter |= unpack "x8 a*", $_;
  }
  pack("NN", $n, $k) . $filter;
}

sub bloom_count($) {
  my ($m, $k, $bits) = unpack "NN %32b*", $_[0];
  $m * -log(1 - $bits/$m) / $k;
}

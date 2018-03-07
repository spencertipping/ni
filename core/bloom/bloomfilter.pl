# Bloom filter library.
# A simple pure-Perl implementation of Bloom filters.

eval {require Digest::MD5; Digest::MD5->import('md5')};

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

sub bloom_from_hex($) {
  pack 'H*', $_[0];
}

sub multihash($$) {
  my @hs;
  push @hs, unpack "Q2", md5($_[0] . scalar @hs) until @hs >= $_[1];
  @hs[0..$_[1]-1];
}

# Destructively adds an element to the filter and returns the filter.
sub bloom_add($$) {
  my ($m, $k) = unpack "QQ", $_[0];
  vec($_[0], $_ % $m + 128, 1) = 1 for multihash $_[1], $k;
  $_[0];
}

sub bloom_contains($$) {
  my ($m, $k) = unpack "NN", $_[0];
  vec($_[0], $_ % $m + 128, 1) || return 0 for multihash $_[1], $k;
  1;
}

# Prehashing variants
# prehash($m, $k, $element) -> "prehash_string"
# bloom_add_prehashed($filter, "prehash_string")
sub bloom_prehash($$$) {
  my ($m, $k) = @_;
  join ",", map $_ % $m, multihash $_[2], $k;
}

sub bloom_add_prehashed($$) {
  vec($_[0], $_ + 128, 1) = 1 for split /,/, $_[1];
  $_[0];
}

sub bloom_contains_prehashed($$) {
  vec($_[0], $_ + 128, 1) || return 0 for split /,/, $_[1];
  1;
}

# Set operations
sub bloom_intersect {
  local $_;
  my ($n, $k, $filter) = unpack "QQa*", shift;
  for (@_) {
    my ($rn, $rk) = unpack "QQ", $_;
    die "cannot intersect two bloomfilters of differing parameters "
      . "($n, $k) vs ($rn, $rk)"
      unless $n == $rn && $k == $rk;
    $filter &= unpack "x16 a*", $_;
  }
  pack("NN", $n, $k) . $filter;
}

sub bloom_union {
  local $_;
  my ($n, $k, $filter) = unpack "QQa*", shift;
  for (@_) {
    my ($rn, $rk) = unpack "QQ", $_;
    die "cannot union two bloomfilters of differing parameters "
      . "($n, $k) vs ($rn, $rk)"
      unless $n == $rn && $k == $rk;
    $filter |= unpack "x16 a*", $_;
  }
  pack("NN", $n, $k) . $filter;
}

sub bloom_count($) {
  my ($m, $k, $bits) = unpack "QQ %64b*", $_[0];
  return -1 if $bits >= $m;     # overflow case (> if %32b runs past end maybe)
  $m * -log(1 - $bits/$m) / $k;
}

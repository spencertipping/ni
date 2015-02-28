NI_MODULE geohash

# 64-bit hex constants in geohash encoder won't work on 32-bit architectures
no warnings 'portable';

our @gh_alphabet = split //, '0123456789bcdefghjkmnpqrstuvwxyz';
our %gh_decode   = map(($gh_alphabet[$_], $_), 0..$#gh_alphabet);

sub gap_bits {
  my ($x) = @_;
  $x |= $x << 16; $x &= 0x0000ffff0000ffff;
  $x |= $x << 8;  $x &= 0x00ff00ff00ff00ff;
  $x |= $x << 4;  $x &= 0x0f0f0f0f0f0f0f0f;
  $x |= $x << 2;  $x &= 0x3333333333333333;
  return ($x | $x << 1) & 0x5555555555555555;
}

sub ungap_bits {
  my ($x) = @_;  $x &= 0x5555555555555555;
  $x ^= $x >> 1; $x &= 0x3333333333333333;
  $x ^= $x >> 2; $x &= 0x0f0f0f0f0f0f0f0f;
  $x ^= $x >> 4; $x &= 0x00ff00ff00ff00ff;
  $x ^= $x >> 8; $x &= 0x0000ffff0000ffff;
  return ($x ^ $x >> 16) & 0x00000000ffffffff;
}

sub ::geohash_encode {
  my ($lat, $lng, $precision) = @_;
  $precision //= 12;
  my $bits = $precision > 0 ? $precision * 5 : -$precision;
  my $gh   = (gap_bits(int(($lat +  90) / 180 * 0x40000000)) |
              gap_bits(int(($lng + 180) / 360 * 0x40000000)) << 1)
             >> 60 - $bits;

  $precision > 0 ? join '', reverse map $gh_alphabet[$gh >> $_ * 5 & 31],
                                        0 .. $precision - 1
                 : $gh;
}

sub ::geohash_decode {
  my ($gh, $bits) = @_;
  unless (defined $bits) {
    # Decode gh from base-32
    $bits = length($gh) * 5;
    my $n = 0;
    $n = $n << 5 | $gh_decode{lc $_} for split //, $gh;
    $gh = $n;
  }
  $gh <<= 60 - $bits;
  return (ungap_bits($gh)      / 0x40000000 * 180 -  90,
          ungap_bits($gh >> 1) / 0x40000000 * 360 - 180);
}

defshortfn 'ghe', \&::geohash_encode;
defshortfn 'ghd', \&::geohash_decode;

NI_MODULE_END

# Fast, portable geohash encoder.
# A port of https://www.factual.com/blog/how-geohashes-work. I'm assuming 64-bit
# int support.

use Scalar::Util qw/looks_like_number/;

our @geohash_alphabet = split //, '0123456789bcdefghjkmnpqrstuvwxyz';
our %geohash_decode   = map(($geohash_alphabet[$_], $_), 0..$#geohash_alphabet);

sub geohash_base32_to_binary($) {
  my $gh_b32 = shift;
  my $i = length $gh_b32;
  my $gh = sum map {$geohash_decode{$_} << 5 * (--$i)} split //, $gh_b32;
  $gh;
}

sub gh58($)
{
  my $gh30 = shift;
  $gh30 = ($gh30 & 0x3fff8000) << 9 | $gh30 & 0x7fff;
  $gh30      &   0x1f00001f
    | ($gh30 &  0x3e00003e0) << 3
    | ($gh30 & 0x7c00007c00) << 6;
}

sub geohash_binary_to_base32($$)
{
  my ($gh, $p) = @_;
  if ($p <= 20)
  {
    $gh = gh58 $gh << $p;
    my $s = pack "N", $gh;
    $s =~ y/\x00-\x31/0123456789bcdefghjkmnpqrstuvwxyz/;
    substr $s, 0, int(($p + 4) / 5);
  }
  elsif ($p <= 40)
  {
    $gh <<= 40 - $p;
    my $w1 = gh58 $gh;
    my $w2 = gh58 $gh >> 30;
    my $s  = pack "N2", $w1 >> 32 & 0xffff | $w2 << 16 & 0xffff0000,
                        $w1 & 0xffffffff;
    $s =~ y/\x00-\x31/0123456789bcdefghjkmnpqrstuvwxyz/;
    substr $s, 0, int(($p + 4) / 5);
  }
  else
  {
    $gh <<= 60 - $p;
    my $w1 = gh58 $gh;
    my $w2 = gh58 $gh >> 30;
    my $s  = pack "N3", $w2 >> 16,
                        $w1 >> 32 & 0xffff | $w2 << 16 & 0xffff0000,
                        $w1 & 0xffffffff;
    $s =~ y/\x00-\x31/0123456789bcdefghjkmnpqrstuvwxyz/;
    substr $s, 0, int(($p + 4) / 5);
  }
}

sub morton_gap($) {
  my ($x) = @_;
  $x |= $x << 16; $x &= 0x0000ffff0000ffff;
  $x |= $x << 8;  $x &= 0x00ff00ff00ff00ff;
  $x |= $x << 4;  $x &= 0x0f0f0f0f0f0f0f0f;
  $x |= $x << 2;  $x &= 0x3333333333333333;
  return ($x | $x << 1) & 0x5555555555555555;
}

sub morton_ungap($) {
  my ($x) = @_;  $x &= 0x5555555555555555;
  $x ^= $x >> 1; $x &= 0x3333333333333333;
  $x ^= $x >> 2; $x &= 0x0f0f0f0f0f0f0f0f;
  $x ^= $x >> 4; $x &= 0x00ff00ff00ff00ff;
  $x ^= $x >> 8; $x &= 0x0000ffff0000ffff;
  return ($x ^ $x >> 16) & 0x00000000ffffffff;
}

sub geohash_encode {
  local $_;
  my ($lat, $lng, $precision) = @_;
  $precision ||= 12;
  my $bits = $precision > 0 ? $precision * 5 : -$precision;
  my $gh   = (morton_gap(int(($lat +  90) / 180 * 0x40000000)) |
              morton_gap(int(($lng + 180) / 360 * 0x40000000)) << 1)
             >> 60 - $bits;

  $precision > 0 ? join '', reverse map $geohash_alphabet[$gh >> $_ * 5 & 31],
                                        0 .. $precision - 1
                 : $gh;
}

sub geohash_tagged_precision {
  my ($gh) = @_;
  $gh &= 0x3fff_ffff_ffff_ffff;
  my $bits = 0;
  for (my $b = 32; $b; $b >>= 1) {
    $bits |= $b if ($gh & ~(-1 << ($bits | $b))) != $gh;
  }
  $bits;
}

sub geohash_decode_tagged {
  my ($gh) = @_;
  $gh &= 0x3fff_ffff_ffff_ffff;
  my $tag_bit_index = geohash_tagged_precision($gh);
  geohash_decode($gh & ~(-1 << $tag_bit_index), $tag_bit_index);
}

sub geohash_decode {
  local $_;
  my ($gh, $bits) = @_;
  unless (defined $bits) {
    return geohash_decode_tagged($gh)
      if looks_like_number $gh && $gh & 0x4000_0000_0000_0000;

    # Decode gh from base-32
    $bits = length($gh) * 5;
    my $n = 0;
    $n = $n << 5 | $geohash_decode{lc $_} for split //, $gh;
    $gh = $n;
  }
  $gh <<= 60 - $bits;
  return (morton_ungap($gh)      / 0x40000000 * 180 -  90,
          morton_ungap($gh >> 1) / 0x40000000 * 360 - 180);
}

BEGIN
{
  *ghe = \&geohash_encode;
  *llg = \&geohash_encode;
  *ghd = \&geohash_decode;
  *gll = \&geohash_decode;
  *g3b = \&geohash_base32_to_binary;
  *gb3 = \&geohash_binary_to_base32;
}

sub to_radians {
  3.1415926535897943284626 * $_[0]/180.0;
}

sub earth_radius_in_units {
  if ($_[0] eq "km") {
    6371} elsif ($_[0] eq "mi") {
    3959} elsif ($_[0] eq "m") {
    6371E3} elsif ($_[0] eq "ft") {
    20903520 }  #3959 * 5280
     else { -1 }
}

sub lat_lon_dist {
  my $units = @_ == 4 ? "km" : pop;
  my ($lat1, $lon1, $lat2, $lon2) = @_;
  my $earth_radius = earth_radius_in_units $units;
  my $phi1 = to_radians $lat1;
  my $phi2 = to_radians $lat2;
  my $d_phi = $phi1 - $phi2;
  my $d_lambda = to_radians ($lon1 - $lon2);
  my $a = sin($d_phi/2)**2 + cos($phi1)*cos($phi2)*(sin($d_lambda/2)**2);
  my $c = 2 * atan2(sqrt($a), sqrt(1-$a));
  $earth_radius * $c
}

sub gh_dist {
  my @lat_lons;
  push @lat_lons, ghd($_[0]), ghd($_[1]), ($_[2] || "km");
  lat_lon_dist @lat_lons;
}

sub geohash_box($;$) {
  my $gh = shift;
  my $northeast_corner = substr($gh . "z" x 12, 0, 12);
  my $southwest_corner = substr($gh . "0" x 12, 0, 12);
  my ($north, $east) = ghd($northeast_corner);
  my ($south, $west) = ghd($southwest_corner);
  ($north, $south, $east, $west);
}

BEGIN { *ghb = \&geohash_box }

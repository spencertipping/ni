# Fast, portable geohash encoder.
# A port of https://www.factual.com/blog/how-geohashes-work that works on 32-bit
# Perl builds.

BEGIN {

use Scalar::Util qw/looks_like_number/;

our @geohash_alphabet = split //, '0123456789bcdefghjkmnpqrstuvwxyz';
our %geohash_decode   = map(($geohash_alphabet[$_], $_), 0..$#geohash_alphabet);

if (1 << 32) {
  *morton_gap = sub($) {
    my ($x) = @_;
    $x |= $x << 16; $x &= 0x0000ffff0000ffff;
    $x |= $x << 8;  $x &= 0x00ff00ff00ff00ff;
    $x |= $x << 4;  $x &= 0x0f0f0f0f0f0f0f0f;
    $x |= $x << 2;  $x &= 0x3333333333333333;
    return ($x | $x << 1) & 0x5555555555555555;
  };

  *morton_ungap = sub($) {
    my ($x) = @_;  $x &= 0x5555555555555555;
    $x ^= $x >> 1; $x &= 0x3333333333333333;
    $x ^= $x >> 2; $x &= 0x0f0f0f0f0f0f0f0f;
    $x ^= $x >> 4; $x &= 0x00ff00ff00ff00ff;
    $x ^= $x >> 8; $x &= 0x0000ffff0000ffff;
    return ($x ^ $x >> 16) & 0x00000000ffffffff;
  };

  *geohash_encode = sub {
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
  };

  *geohash_tagged_precision = sub {
    my ($gh) = @_;
    $gh &= 0x3fff_ffff_ffff_ffff;
    my $bits = 0;
    for (my $b = 32; $b; $b >>= 1) {
      $bits |= $b if ($gh & ~(-1 << ($bits | $b))) != $gh;
    }
    $bits;
  };

  *geohash_decode_tagged = sub {
    my ($gh) = @_;
    $gh &= 0x3fff_ffff_ffff_ffff;
    my $tag_bit_index = geohash_tagged_precision($gh);
    geohash_decode($gh & ~(-1 << $tag_bit_index), $tag_bit_index);
  };

  *geohash_decode = sub {
    local $_;
    my ($gh, $bits) = @_;
    return geohash_decode_tagged($gh)
      if looks_like_number $gh and $gh !~ /[0-9a-z]{1,12}/ and $gh & 0x4000_0000_0000_0000;
    unless (defined $bits) {
      # Decode gh from base-32
      $bits = length($gh) * 5;
      my $n = 0;
      $n = $n << 5 | $geohash_decode{lc $_} for split //, $gh;
      $gh = $n;
    }
    $gh <<= 60 - $bits;
    return (morton_ungap($gh)      / 0x40000000 * 180 -  90,
            morton_ungap($gh >> 1) / 0x40000000 * 360 - 180);
  };
} else {
  *morton_gap = sub($) {
    my ($x) = @_;
    $x |= $x << 8;  $x &= 0x00ff00ff;
    $x |= $x << 4;  $x &= 0x0f0f0f0f;
    $x |= $x << 2;  $x &= 0x33333333;
    return ($x | $x << 1) & 0x55555555;
  };

  *morton_ungap = sub($) {
    my ($x) = @_;  $x &= 0x55555555;
    $x ^= $x >> 1; $x &= 0x33333333;
    $x ^= $x >> 2; $x &= 0x0f0f0f0f;
    $x ^= $x >> 4; $x &= 0x00ff00ff;
    return ($x ^= $x >> 8) & 0x0000ffff;
  };

  *geohash_encode = sub {
    local $_;
    my ($lat, $lng, $precision) = (@_, 12);
    my $unit_lat = ($lat + 90)  / 180;
    my $unit_lng = ($lng + 180) / 360;
    my $high_30  = morton_gap($unit_lat * 0x8000)
                 | morton_gap($unit_lng * 0x8000) << 1;
    my $low_30   = morton_gap($unit_lat * 0x40000000 & 0x7fff)
                 | morton_gap($unit_lng * 0x40000000 & 0x7fff) << 1;

    my $gh12 = join '', map($geohash_alphabet[$high_30 >> 30 - 5*$_ & 31], 1..6),
                        map($geohash_alphabet[$low_30  >> 30 - 5*$_ & 31], 1..6);
    substr $gh12, 0, $precision;
  };

  *geohash_decode = sub {
    local $_;
    my $gh12 = "$_[0]s" . "0" x 11;
    my ($low_30, $high_30) = (0, 0);
    for (0..5) {
      $low_30  = $low_30  << 5 | $geohash_decode{lc substr $gh12, $_ + 6, 1};
      $high_30 = $high_30 << 5 | $geohash_decode{lc substr $gh12, $_    , 1};
    }
    my $lat_int = morton_ungap($low_30)      | morton_ungap($high_30)      << 15;
    my $lng_int = morton_ungap($low_30 >> 1) | morton_ungap($high_30 >> 1) << 15;
    ($lat_int / 0x40000000 * 180 - 90, $lng_int / 0x40000000 * 360 - 180);
  };
}

*ghe = \&geohash_encode;
*ghd = \&geohash_decode;

*gh_box = sub {
  local $_;
  my $gh = shift;
  my $northeast_corner = substr($gh . "z" x 12, 0, 12);
  my $southwest_corner = substr($gh . "0" x 12, 0, 12);
  my ($north, $east) = ghd($northeast_corner);
  my ($south, $west) = ghd($southwest_corner);
  ($north, $south, $east, $west);
  };

*ghb = \&gh_box;
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

# Fast, portable geohash encoder... AND MORE!
# A port of https://www.factual.com/blog/how-geohashes-work. 
# I'm assuming 64-bit int support.

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

sub to_radians {
  3.1415926535897943284626 * $_[0]/180.0;
}

our %earth_radius_in_units = ("km" => 6371, "mi" =>3959, 
                              "m" =>6371E3, "ft"=>3959 * 5280);

sub lat_lon_dist {
  my $units = @_ == 4 ? "km" : pop;
  my ($lat1, $lon1, $lat2, $lon2) = @_;
  my $earth_radius = $earth_radius_in_units{$units};
  my $phi1 = to_radians $lat1;
  my $phi2 = to_radians $lat2;
  my $d_phi = $phi1 - $phi2;
  my $d_lambda = to_radians ($lon1 - $lon2);
  my $a = sin($d_phi/2)**2 + cos($phi1)*cos($phi2)*(sin($d_lambda/2)**2);
  my $c = 2 * atan2(sqrt($a), sqrt(1-$a));
  $earth_radius * $c
}

sub gh_dist_exact {
  my @lat_lons;
  push @lat_lons, gll($_[0]), gll($_[1]), ($_[2] || "km");
  lat_lon_dist @lat_lons;
}

sub geohash_box($;$) {
  my $gh = shift;
  my $northeast_corner = substr($gh . "z" x 12, 0, 12);
  my $southwest_corner = substr($gh . "0" x 12, 0, 12);
  my ($north, $east) = gll($northeast_corner);
  my ($south, $west) = gll($southwest_corner);
  ($north, $south, $east, $west);
}

sub geohash_cardinal_direction {
  my ($gh1, $gh2, $precision) = @_;
  my $n = ($gh2 & 0x5555_5555_5555_5555) > ($gh1 & 0x5555_5555_5555_5555);
  my $s = ($gh1 & 0x5555_5555_5555_5555) > ($gh2 & 0x5555_5555_5555_5555);
  my $e = ($gh2 & 0xaaaa_aaaa_aaaa_aaaa) > ($gh1 & 0xaaaa_aaaa_aaaa_aaaa);
  my $w = ($gh1 & 0xaaaa_aaaa_aaaa_aaaa) > ($gh2 & 0xaaaa_aaaa_aaaa_aaaa);
  $n - $s, $e - $w;
}

sub most_significant_even_bit_index {
  my $x = shift;
  my $index = 2;
  ($x & 0xffff_ffff_0000_0000) && ($index += 32) && ($x >>= 32);
  ($x & 0xffff_0000)           && ($index += 16) && ($x >>= 16);
  ($x & 0xff00)                && ($index += 8)  && ($x >>= 8 );
  ($x & 0xf0)                  && ($index += 4)  && ($x >>= 4 );
  ($x & 0xc)                   && ($index += 2);
  $index;
}


sub morton_generate($$) {
  my ($n_bits, $offset) = @_;
  map {morton_ungap $_ >> $offset} 0..(2**$n_bits - 1);
}

our $MORTON_PRECISION = 10;
our @morton_eastings  = morton_generate($MORTON_PRECISION, 0);
our @morton_northings = morton_generate($MORTON_PRECISION, 1);
our $latitude_meters  = $earth_radius_in_units{"m"} * to_radians(1);
our @longitude_meters = map {$latitude_meters * cos(to_radians($_ * 90/16))} 0..16; 
our @morton_longitude_lengths = map {$longitude_meters[abs $_ - 16] } @morton_northings;


sub gh_dist_approx {
  # Compute approximate distance along the earth
  # without branching, map, or inverse trig functions
  # BIT HACKS BIT HACKS BIT HACKS
#  printf "%s\n", join "\t", @morton_northings;
  my ($gh1, $gh2, $precision) = @_;
  my $diff = ($gh1 ^ $gh2) << (60 - $precision);
  # need to be in the same 10-bit geohash
  return gh_dist gb3 $gh1, gb3 $gh2 if $diff & 0x0ffc_0000_0000_0000;
  my $diff_msb_index = most_significant_even_bit_index $diff;
#  printf "%s: %d\n", "Most significant difference at position:", $diff_msb_index - $precision;
  my $ms_diff = $diff  >> max 0, $diff_msb_index - $MORTON_PRECISION;
#  printf "%s %d %b\n", "most significant gh diff @ position:", $diff_msb_index, $ms_diff;
  my $northing = $morton_northings[$ms_diff]; 
  my $easting  = $morton_eastings[$ms_diff];
#  print("Northing/Easting: $northing\t$easting\n");
  my $north_degrees = $northing * 180 * 2**(($diff_msb_index - $MORTON_PRECISION - 60)/2);
  my $east_degrees  = $easting  * 360 * 2**(($diff_msb_index - $MORTON_PRECISION - 60)/2);
#  print("North/East Degrees: $north_degrees\t$east_degrees\n");
  # longitude is a different length at different latitudes 
  my $longitude_meters = $morton_longitude_lengths[$gh1 >> ($precision - $MORTON_PRECISION)];

  print("Longitude Length: $longitude_meters\n");
  my $dist = sqrt (($north_degrees * $latitude_meters)**2 + ($east_degrees * $longitude_meters)**2);
  $dist; 
}



BEGIN
{
  *ghe = \&geohash_encode;
  *llg = \&geohash_encode;
  *ghd = \&geohash_decode;
  *gll = \&geohash_decode;
  *g3b = \&geohash_base32_to_binary;
  *gb3 = \&geohash_binary_to_base32;
  *ghb = \&geohash_box;
  *gh_dist_a = \&gh_dist_approx;
  *gh_dist = \&gh_dist_exact;
}

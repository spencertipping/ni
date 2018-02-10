# Geohash functions
ni has fairly extensive support for
[geohashes](https://en.wikipedia.org/wiki/Geohash), both in base-32 and in
binary forms. It uses the [Factual bitwise
algorithm](https://www.factual.com/blog/how-geohashes-work/) and some
surrounding Perl-specific optimizations for base-32 transcoding.

There are a few operators and functions that operate on geohashes:

- Perl context functions
  - `$gh = ghe/llg/geohash_encode($lat, $lng, $precision)`: encode a geohash
  - `($lat, $lng) = ghd/gll/geohash_decode($gh [, $bits])`: decode a geohash
  - `$dist = gh_dist_exact($gh1, $gh2, unit = "km")`
  - `($n, $s, $e, $w) = ghb/geohash_box($gh)`
- Cell operators
  - `,g`: encode comma-delimited column values to a geohash
  - `,G`: decode a column of geohashes to comma-delimited lat/lng

## Geohashes in the perl context
```bash
$ ni nE4p'my ($lat, $lng) = (rand() * 180 - 90, rand() * 360 - 180);
          my $bits        = int clip 1, 60, rand(61);
          my $b32_digits  = int(($bits + 4) / 5);
          my $gh_base32   = ghe $lat, $lng, $b32_digits;  # positive = letters
          my $gh_binary   = ghe $lat, $lng, -$bits;       # negative = bits
          my $gh2_b32     = ghe ghd($gh_base32), $b32_digits;
          my $gh2_bin     = ghe ghd($gh_binary, $bits), -$bits;
          my $b32_ok      = $gh2_b32 eq $gh_base32;
          my $bin_ok      = $gh2_bin == $gh_binary;

          r $b32_ok ? "B32 OK" : "B32 FAIL $lat $lng $gh_base32 $gh2_b32";
          r $bin_ok ? "BIN OK" : "BIN FAIL $lat $lng $gh_binary $gh2_bin"' \
     gcfB.
B32 OK
BIN OK
```

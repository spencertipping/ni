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
          my $gh_base32   = ghe $lat, $lng, 12;   # positive = base32 letters
          my $gh_binary   = ghe $lat, $lng, -60;  # negative = bits

          r "base32 works" if ghe(ghd($gh_base32)) eq $gh_base32;
          r "binary works" if ghe(ghd($gh_binary, 60), 60) == $gh_binary' \
     gc
10000	base32 works
10000	binary works
```

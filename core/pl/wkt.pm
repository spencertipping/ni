# WKT and polygons (ported from nfu)

sub line_opposite
{
  # Returns true if two points are on opposite sides of the line starting at
  # (x0, y0) and whose direction is (dx, dy).
  my ($x0, $y0, $dx, $dy, $x1, $y1, $x2, $y2) = @_;
  return (($x1 - $x0) * $dy - ($y1 - $y0) * $dx)
       * (($x2 - $x0) * $dy - ($y2 - $y0) * $dx) < 0;
}

sub evens(@) {local $_; @_[map $_ * 2,     0 .. $#_ >> 1]}
sub odds(@)  {local $_; @_[map $_ * 2 + 1, 0 .. $#_ >> 1]}

sub parse_rings
{
  +{
    rings  => [map [map $_ + 0, @$_], @_],
    ylimit => 1 + max(map max(@$_), @_),
    bounds => [min(map evens(@$_), @_), max(map evens(@$_), @_),
               min(map odds(@$_),  @_), max(map odds(@$_),  @_)],
  }
}

sub parse_wkt
{
  parse_rings map [/([-+eE0-9.]+)\s+([-+eE0-9.]+)/g],
              split /\)\s*,\s*\(/, shift;
}

sub parse_wkb
{
  # Parses either hex or binary. The first byte of WKB is an endianness
  # indicator, so if we see a hex digit we know to decode.
  my $wkb   = $_[0] =~ /^[0-9a-fA-F]/ ? pack "H*", shift : shift;
  my $float = $wkb =~ /^\x01/ ? 'd<' : 'd>';
  my $long  = $wkb =~ /^\x01/ ? 'V'  : 'N';

  my $type     = unpack "x $long", $wkb;
  my $has_srid = $type & 0x20000000;
  my $prefix   = $has_srid ? 9 : 5;
  $type &= 0xff;

  # Parse linestrings, polygons, multilinestrings, and multipolygons. Assume
  # constant endianness throughout the WKB; anything else would be insane.
  my $template =
      $type == 2 || $type == 3 ? "x$prefix $long/(   $long/($long X4 $long/($float$float)))"
    : $type == 5 || $type == 6 ? "x$prefix $long/(x5 $long/($long X4 $long/($float$float)))"
    :                            die "parse_wkb: unknown type $type";

  my @pts = unpack $template, $wkb;

  # Now @pts contains length-prefixed float pairs. We need to convert those to
  # array refs.
  my @rings;
  my $n;
  for (my $i = 0; $i < @pts - 2; $i += $n*2)
  {
    $n = $pts[$i];
    push @rings, [@pts[$i + 1 .. $i + $n*2]];
  }

  parse_rings @rings;
}

sub in_poly
{
  # Returns true if a point resides in the given parsed polygon.
  my ($x, $y, $parsed) = @_;
  my $ylimit = $parsed->{ylimit};
  my @bounds = @{$parsed->{bounds}};
  return 0 if $x < $bounds[0] || $x > $bounds[1]
           || $y < $bounds[2] || $y > $bounds[3];

  my $hits = 0;
  for my $r (@{$parsed->{rings}}) {
    my ($lx, $ly) = @$r[0, 1];
    for (my $i = 2; $i < @$r; $i += 2) {
      my $cx = $$r[$i];
      my $cy = $$r[$i + 1];
      ++$hits if $lx <= $x && $x < $cx || $lx >= $x && $x > $cx
             and line_opposite $lx, $ly, $cx - $lx, $cy - $ly,
                               $x, $y, $x, $ylimit;
      $lx = $cx;
      $ly = $cy;
    }
  }
  $hits & 1;
}

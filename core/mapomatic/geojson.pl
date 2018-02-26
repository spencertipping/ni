# GeoJSON functions
# Converts WKTs and other things to geoJSON for export to Map-O-Matic.

use Scalar::Util qw/looks_like_number/;

# The universal operator here is "geojsonify", which takes inputs in a few
# different formats and emits one line of geoJSON each. Specifically, these
# formats are supported:
#
#   lat lng ...                     # render a point
#   lat,lng ...                     # ditto
#   lat,lng +lat,lng ...            # render a point with a delta
#   POINT(...) ...                  # and other WKT formats
#   POLYGON(...) ...
#   9q5c ...                        # base-32 geohash
#   6114200779206244058 ...         # tagged geohash
#   010600... ...                   # WKB
#   {...} ...                       # geoJSON
#
# gh60s will be rendered as points, whereas anything less precise will be a
# bounding polygon.
#
# You can specify other attributes in the columns rightwards of the geometry.
# Those can be any of the following:
#
#   name=value                      # stuff something into properties
#   {"name":"value",...}            # merge into properties
#   #341802                         # set rendering color
#   arbitrary text                  # set title attribute so you get a tooltip
#
# The geoJSON lines coming out of this operator are just the individual
# features. To render as geoJSON for real, you'll want to collect these lines
# and wrap with the boilerplate:
#
# {
#   "type": "FeatureCollection",
#   "features": [
#     <your-geojson-features>
#   ]
# }
#
# ni's mapomatic operator does all of this for you.

use constant geojson_point_gen => gen '{"type":"Point","coordinates":[%x,%y]}';
use constant geojson_vector_gen => gen
  '{"type":"LineString","coordinates":[[%x1,%y1],[%x2,%y2]]}';

use constant geojson_box_gen => gen
    '{'
  .   '"type":"Polygon",'
  .   '"coordinates":['
  .     '[[%w,%s],[%e,%s],[%e,%n],[%w,%n],[%w,%s]]'
  .   ']'
  . '}';

use constant geojson_polygon_gen => gen
    '{'
  .   '"type":"Polygon",'
  .   '"coordinates":[%linear_rings]'
  . '}';

# Individual geojson parsing cases
sub geojson_point
{
  my ($lat, $lng) = @_;
  geojson_point_gen->(x => $lng, y => $lat);
}

sub geojson_vector
{
  my ($lat1, $lng1, $lat2, $lng2) = @_;
  geojson_vector_gen->(x1 => $lng1, y1 => $lat1,
                       x2 => $lng2, y2 => $lat2);
}

sub geojson_box
{
  my ($n, $s, $e, $w) = @_;
  geojson_box_gen->(n => $n, s => $s, e => $e, w => $w);
}

use constant geojson_ring_gen      => gen '[%points]';
use constant geojson_ringpoint_gen => gen '[%x,%y]';

sub geojson_polygon
{
  my ($wkx) = @_;
  my $rings = ($wkx =~ /^[A-Za-z]/ ? parse_wkt $wkx : parse_wkb $wkx)->{rings};
  my $json_rings = join",", map {
    my $lr = $_;
    my @ps;
    for (my $i = 0; $i + 1 < $#$lr; $i += 2)
    {
      push @ps, geojson_ringpoint_gen->(x => $$lr[$i], y => $$lr[$i + 1]);
    }
    geojson_ring_gen->(points => join",", @ps);
  } @$rings;
  geojson_polygon_gen->(linear_rings => $json_rings);
}

# Property extraction
sub geojson_props
{
  my $props = {title => ''};
  for (@_)
  {
    %$props = (%$props, %{json_decode $_}), next if /^{/;
    $$props{$1} = $2, next                       if /([^=]+)=(.*)$/;
    $$props{'marker-color'}
      = $$props{'stroke'}
      = $$props{'fill'} = $_, next               if /^#/;
    $$props{title} .= $_;
  }
  $props;
}

# geojson_parse_geometry(F_) -> ($geometry_obj, $properties_obj)
sub geojson_parse
{
  # Detect obvious cases like WKT, WKB, and JSON
  if ($_[0] =~ /^{/)
  {
    my $decoded = json_decode $_[0];
    (json_encode $$decoded{geometry}, $$decoded{properties});
  }
  elsif ($_[0] =~ /^POINT/)
  {
    my ($lng, $lat) = $_[0] =~ /[-+0-9.eE]+/g;
    return (geojson_point($lat, $lng), geojson_props(@_[1..$#_]));
  }
  elsif ($_[0] =~ /^M|^P/)
  {
    return (geojson_polygon($_[0]), geojson_props(@_[1..$#_]));
  }
  elsif ($_[0] =~ /^([-+0-9.eE]+),([-+0-9.eE]+)$/)
  {
    my ($lat1, $lng1) = ($1, $2);

    # Do we have another lat/lng as the second arg?
    if ($_[1] =~ /^\+([-+0-9.eE]+),([-+0-9.eE]+)$/)
    {
      my ($lat2, $lng2) = ($1, $2);
      return (geojson_vector($lat1, $lng1, $lat2, $lng2),
              geojson_props(@_[2..$#_]));
    }
    else
    {
      # Just a point
      return (geojson_point($lat1, $lng1), geojson_props(@_[1..$#_]));
    }
  }
  elsif ($_[0] =~ /^0[01](?:0000000[2356]|0[2356]000000)[0-9A-Fa-f]{4,}$/)
  {
    # WKB
    return (geojson_polygon($_[0]), geojson_props(@_[1..$#_]));
  }
  elsif (looks_like_number $_[0] && $_[0] & 1 << 62)
  {
    # Tagged geohash
    my $prec = geohash_tagged_precision $_[0];
    if ($prec == 60)
    {
      my ($lat, $lng) = geohash_decode($_[0]);
      return (geojson_point($lat, $lng), geojson_props(@_[1..$#_]));
    }
    else
    {
      my ($n, $s, $e, $w) = geohash_box(gb3 $_[0] & 0x0fff_ffff_ffff_ffff, $prec);
      return (geojson_box($n, $s, $e, $w), geojson_props(@_[1..$#_]));
    }
  }
  elsif (looks_like_number $_[0] && $_[0] >= -90 && $_[0] <= 90)
  {
    # A latitude, so assume the next field is the longitude
    return (geojson_point($_[0], $_[1]), geojson_props(@_[2..$#_]));
  }
  elsif ($_[0] =~ /[0-9a-z]+/)
  {
    # base-32 geohash
    my $geom = length $_[0] == 12 ? geojson_point(geohash_decode $_[0])
                                  : geojson_box(geohash_box $_[0]);
    return ($geom, geojson_props @_[1..$#_]);
  }
  else
  {
    die "geojson_parse: unknown format for row @_";
  }
}

use constant geojson_row_gen => gen
  '{"type":"Feature","geometry":%geom,"properties":%props}';

defoperator geojsonify =>
q{
  while (<STDIN>)
  {
    chomp;
    my ($geom, $props) = geojson_parse(split /\t/);
    print geojson_row_gen->(geom  => $geom,
                            props => json_encode($props)), "\n";
  }
};

defshort '/geojsonify', pmap q{geojsonify_op}, pnone;

# Map-O-Matic
# Runs a webserver that uses GeoJSON to render a map.

use constant geojson_html_gen => gen <<'EOF';
<!DOCTYPE html>
<html>
<head>
<meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no' />
<style>
body { margin:0; padding:0; }
#map { position:absolute; top:0; bottom:0; width:100%; }
.marker-properties {
  border-collapse:collapse;
  font-size:11px;
  border:1px solid #eee;
  margin:0;
}
.marker-properties th {
  white-space:nowrap;
  border:1px solid #eee;
  padding:5px 10px;
}
.marker-properties td {
  border:1px solid #eee;
  padding:5px 10px;
}
.marker-properties tr:last-child td,
.marker-properties tr:last-child th {
  border-bottom:none;
}
.marker-properties tr:nth-child(even) th,
.marker-properties tr:nth-child(even) td {
  background-color:#f7f7f7;
}
</style>
<script src='//api.tiles.mapbox.com/mapbox.js/v2.2.2/mapbox.js'></script>
<script src='//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js' ></script>
<link href='//api.tiles.mapbox.com/mapbox.js/v2.2.2/mapbox.css' rel='stylesheet' />
</head>
<body>
<div id='map'></div>
<script type='text/javascript'>
L.mapbox.accessToken = '%mapbox_key';
var map = L.mapbox.map('map');

L.mapbox.tileLayer('%mapbox_tileset').addTo(map);

function showProperties(l) {
  var properties = l.toGeoJSON().properties;
  var table = document.createElement('table');
  table.setAttribute('class', 'marker-properties display')
  for (var key in properties) {
    var tr = createTableRows(key, properties[key]);
    table.appendChild(tr);
  }
  if (table) l.bindPopup(table);
}

function createTableRows(key, value) {
  var tr = document.createElement('tr');
  var th = document.createElement('th');
  var td = document.createElement('td');
  key = document.createTextNode(key);
  value = document.createTextNode(value);
  th.appendChild(key);
  td.appendChild(value);
  tr.appendChild(th);
  tr.appendChild(td);
  return tr
}

geojson_callback = function(geojson) {
  var geojsonLayer = L.mapbox.featureLayer(geojson).addTo(map);
  var bounds = geojsonLayer.getBounds();
  if (bounds.isValid()) {
    map.fitBounds(geojsonLayer.getBounds());
  } else {
    map.setView([0, 0], 2);
  }
  geojsonLayer.eachLayer(showProperties);
};
</script>
<script id='geojson'>
geojson_callback(%geojson);
$('#geojson').remove();
</script>
</body>
</html>
EOF

use constant geojson_container_gen => gen
  '{"type":"FeatureCollection","features":[%features]}';

use constant mapbox_key_default => 'pk.eyJ1Ijoic3BlbmNlcnRpcHBpbmciLCJhIjoiY2plNGducGNxMTR3cTJycnF1bGRkYmJ0NiJ9.aGaYbtzy_cSYfuQ0fawfTQ';
defconfenv 'mapbox/key',     NI_MAPBOX_KEY     => undef;
defconfenv 'mapbox/tileset', NI_MAPBOX_TILESET => 'mapbox.streets';

sub mapomatic_server {
  my ($port, $geojson) = @_;

  my $key = conf 'mapbox/key';
  unless (defined $key)
  {
    print "WARNING\n";
    print "  You're using ni's builtin Mapbox key, which is shared across\n";
    print "  all ni users. To avoid hitting the free-tier quota, you should\n";
    print "  head to mapbox.com and generate a new key. Keys are free and\n";
    print "  give you about 50000 map views per month. Once you have a key,\n";
    print "  add this line to your .bashrc:\n";
    print "\n";
    print "  export NI_MAPBOX_KEY='<your mapbox access token>'\n";
    print "\n";
    print "  For example:\n";
    print "\n";
    print "  export NI_MAPBOX_KEY='pk.eyJ1Ijoic3BlbmNlcnRpcHBpbmciLCJhIjoiY2plNGducGNxMTR3cTJycnF1bGRkYmJ0NiJ9.aGaYbtzy_cSYfuQ0fawfTQ'\n";
    print "\n";
    print "  You can verify that ni is using your key by running this:\n";
    print "  \$ ni '\$mapbox/key'\n";
    print "\n";

    $key = 'pk.eyJ1Ijoic3BlbmNlcnRpcHBpbmciLCJhIjoiY2plNGducGNxMTR3cTJycnF1bGRkYmJ0NiJ9.aGaYbtzy_cSYfuQ0fawfTQ';
  }

  $|++;
  http $port, sub {
    my ($url, $req, $reply) = @_;
    return print "http://localhost:$port/\n" unless defined $reply;
    return http_reply $reply, 200,
             geojson_html_gen->(geojson        => $geojson,
                                mapbox_key     => $key,
                                mapbox_tileset => conf 'mapbox/tileset')
      if $url eq '/';
    return http_reply $reply, 404, $url;
  };
}

defoperator mapomatic => q{
  mapomatic_server 32768, geojson_container_gen->(features => join",", <STDIN>);
};

defshort '/MM',  pmap q{[geojsonify_op, mapomatic_op]}, pnone;

# Map-O-Matic
# Runs a webserver that uses GeoJSON to render a map.

use constant geojson_html_gen => gen <<'EOF';
<!DOCTYPE html>
<!-- NB: code template swiped from geojson.io, with my mapbox access key -->

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
L.mapbox.accessToken = 'pk.eyJ1Ijoic3BlbmNlcnRpcHBpbmciLCJhIjoiY2plNGducGNxMTR3cTJycnF1bGRkYmJ0NiJ9.aGaYbtzy_cSYfuQ0fawfTQ';
var map = L.mapbox.map('map');

L.mapbox.tileLayer('mapbox.streets').addTo(map);

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
  geojsonLayer.eachLayer(function(l) {
      showProperties(l);
  });
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

sub mapomatic_server {
  my ($port, $geojson) = @_;
  $|++;
  http $port, sub {
    my ($url, $req, $reply) = @_;
    return print "http://localhost:$port/\n" unless defined $reply;
    return http_reply $reply, 200, geojson_html_gen->(geojson => $geojson) if $url eq '/';
    return http_reply $reply, 404, $url;
  };
}

defoperator mapomatic => q{
  mapomatic_server 32768, geojson_container_gen->(features => join",", <STDIN>);
};

defshort '/MM',  pmap q{[geojsonify_op, mapomatic_op]}, pnone;

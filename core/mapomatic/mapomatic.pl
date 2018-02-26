# Map-O-Matic
# Generates a bl.ocks.org URL that points to a geojson.io map.

use constant geojson_page_json => json_encode <<'EOF';
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

$.getJSON('map.geojson', function(geojson) {
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
});

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

</script>
</body>
</html>
EOF

# NB: we do indeed want to escape all of the quote marks in these things. This
# is an optimization that makes it much faster to format the JSON that
# gist.github.com expects. (See the mapomatic operator for some details.)

use constant geojson_container_gen => gen
  '{\"type\":\"FeatureCollection\",\"features\":[%features]}';

use constant mapomatic_upload_gen => gen
    '{'
  .   '"description":"mapomatic",'
  .   '"files":{'
  .     '"index.html":{"content":%page},'
  .     '"map.geojson":{"content":"%escaped_geojson"}'
  .   '}'
  . '}';

defoperator mapomatic => q{
  my @escaped_lines;
  while (<STDIN>)
  {
    chomp;
    s/([\\\\"])/\\\\$1/g;
    push @escaped_lines, $_;
  }
  my $geojson_features =
    geojson_container_gen->(features => join",", @escaped_lines);

  my ($in, $out) = sioproc {sh 'curl -sS -d @- https://api.github.com/gists'};
  print $in mapomatic_upload_gen->(
    page            => geojson_page_json,
    escaped_geojson => $geojson_features);
  close $in;
  my $out_json = join'', <$out>;
  my ($gist_id) = $out_json =~ /"id"\s*:\s*"([0-9a-f]+)"/;
  print "http://bl.ocks.org/anonymous/raw/$gist_id";
};

defshort '/MM',  pmap q{[geojsonify_op, mapomatic_op]}, pnone;

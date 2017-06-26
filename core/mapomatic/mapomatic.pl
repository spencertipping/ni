# Map-O-Matic
# Generates a data: url that maps points on a page using Leaflet.

sub mapomatic_compress {
  local $_ = shift;
  s/^\s+//mg; s/\v+//g; s/\s+/ /g;
  $_;
}

use constant mapomatic_header => <<'EOF';
<html>
<head>
  <link rel="stylesheet"href="http://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.3/leaflet.css"/>
  <script src="http://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.3/leaflet.js"></script>
  <script src="http://code.jquery.com/jquery-1.11.3.min.js"></script>
  <style>body {margin: 0}</style>
</head>
<body id='map'>
  <script type='geodata'>
EOF

use constant mapomatic_footer => <<'EOF';
  </script>
  <script>
  $(function () {
    var rf = function () {
      if ($('#map').height() !== $(window).height())
        $('#map').height($(window).height());
    };
    $(window).resize(rf);
    setTimeout(rf, 100);
    var m = L.map('map');
    L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png',
                {attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'}).addTo(m);
    var sy = 0, sx = 0;
    var ls = $('script[type="geodata"]').text().replace(/^\s*|\s*$/g, '').split(/~/);
    for (var i = 0, l = ls.length; i < l; ++i) {
      var ps = ls[i].split(/\s+/);
      var ll = [+ps[0], +ps[1]];
      L.marker(ll).addTo(m).bindPopup(ps.slice(2).join(' '));
      sy += ll[0];
      sx += ll[1];
    }
    m.setView([sy / ls.length, sx / ls.length], 4);
  });
  </script>
</body>
</html>
EOF

defoperator mapomatic => q{
  eval {require MIME::Base64};
  my $encoded_points = join "~", map mapomatic_compress($_), <STDIN>;
  print "data:text/html;base64," . MIME::Base64::encode_base64(
    mapomatic_compress(mapomatic_header)
      . $encoded_points
      . mapomatic_compress(mapomatic_footer)) . "\n\0";
};

defshort '/MM',  pmap q{mapomatic_op}, pnone;

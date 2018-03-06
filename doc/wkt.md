# WKT and WKB
ni has some support for polygon geometry using WKT and WKB; in particular, it
supports visualization (using [Map-O-Matic](../core/mapomatic/mapomatic.pl)) and
[point-in-poly queries](../core/pl/wkt.pl).

## Map-O-Matic
Map-O-Matic provides instant map visualization for various types of geographic
data (via MapBox and GeoJSON):

- Points
  - TSV `lat lng` pairs
  - CSV `lat,lng` pairs
  - TSV `lat1,lng1 >lat2,lng2` vectors
  - TSV `lat1,lng1 +lat2,lng2` delta vectors
- WKT/WKB
  - `POINT(X Y)` WKT
  - `POLYGON((X1 Y1, ...))` WKT/WKB
  - `MULTIPOLYGON(((...)))` WKT/WKB
  - `01....`: WKB (e.g. PostGIS binary format)
- Geohashes
  - Base-32 geohashes as boxes or points
  - Tagged binary geohashes as boxes or points
- GeoJSON

You can mix the above types within a datastream. For example, if you wanted to
visualize some geohashes you could use a file like this:

```sh
$ cat > geometries <<EOF
9q5c
9q5cc2
dr5r
34,-118
EOF

$ ni geometries geojsonify        # convert the above to GeoJSON
{"type":"Feature","geometry":{"type":"Polygon","coordinates":[[[-118.4765625,33.92578125],[-118.125000335276,33.92578125],[-118.125000335276,34.1015623323619],[-118.4765625,34.1015623323619],[-118.4765625,33.92578125]]]},"properties":{"title":""}}
{"type":"Feature","geometry":{"type":"Polygon","coordinates":[[[-118.421630859375,34.0576171875],[-118.410644866526,34.0576171875],[-118.410644866526,34.0631101839244],[-118.421630859375,34.0631101839244],[-118.421630859375,34.0576171875]]]},"properties":{"title":""}}
{"type":"Feature","geometry":{"type":"Polygon","coordinates":[[[-74.1796875,40.60546875],[-73.8281253352761,40.60546875],[-73.8281253352761,40.7812498323619],[-74.1796875,40.7812498323619],[-74.1796875,40.60546875]]]},"properties":{"title":""}}
{"type":"Feature","geometry":{"type":"Point","coordinates":[-118,34]},"properties":{"title":""}}

$ ni geometries MM                # run the map-o-matic webserver
WARNING
  You're using ni's builtin Mapbox key, which is shared across
  all ni users. To avoid hitting the free-tier quota, you should
  head to mapbox.com and generate a new key. Keys are free and
  give you about 50000 map views per month. Once you have a key,
  add this line to your .bashrc:

  export NI_MAPBOX_KEY='<your mapbox access token>'

  For example:

  export NI_MAPBOX_KEY='pk.eyJ1Ijoic3BlbmNlcnRpcHBpbmciLCJhIjoiY2plNGducGNxMTR3cTJycnF1bGRkYmJ0NiJ9.aGaYbtzy_cSYfuQ0fawfTQ'

  You can verify that ni is using your key by running this:
  $ ni '$mapbox/key'

http://localhost:32768/           # open this link to see the map
```

![image](http://storage7.static.itmages.com/i/18/0306/h_1520339724_2796459_7968a9cf8d.png)

![image](http://storage1.static.itmages.com/i/18/0306/h_1520339761_4850348_0d5ca9b5f8.png)

### Geometry metadata
You can assign attributes to geometries by appending TSV columns to the right.
ni understands a few different types of metadata values:

- `name=value`: set a GeoJSON property
- `{"name":"value",...}`: merge properties into GeoJSON properties map
- `#rrggbb`: set the color of the geometry
- `0.1`: set color to hue, interpolated from 0 (orange) to 1 (purple)
- `text`: append to the `title` attribute, which results in a tooltip

These properties are visible in a popup table if you click on geometries.

For example, let's look at some geohashes around Albuquerque:

```sh
$ ni i[9whp 9whp '#fa4'] \
     i[9wk2 9wk2 '#797'] \
     i[9wk0 this_is_geohash=9wk0] \
     MM
```

![image](http://storage1.static.itmages.com/i/18/0306/h_1520340382_4974654_ce07e420d6.png)

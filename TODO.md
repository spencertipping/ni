### Join operator
This doesn't exist yet. nfu had one but it was awful; this one should support
arbitrary-column joins and not rearrange any columns in the result.

### Data closures
Incorporate data into the image. If it's too large, map it to a tempfile.

### `defshort` should create parsers

### Ops and parsers should be self-documenting

### CLI functions (aliases)

### Autofilled directories
e.g. `/mc9drc24` for `/mnt/cache/t9/data/reddit-comments-2015.lz4`

### Configuration variables

### Document+test dev options
`--explain`, `--dev/parse-one`, etc. Maybe also come up with a better output
format for things like `//options` and `--dev/parse`. Also document why some
options are `--` and others are `//`.

### JSPlot
#### Labeled points
#### Labeled axes/grids/etc
Must be client-side; this way it can happen after autoranging and during zooms.

#### Custom autorange
#### Outlier removal
#### Precise zooming, view history
#### Options to reset/generate view matrix
#### Client-side axis reordering
Should be trivial since the data is all there.

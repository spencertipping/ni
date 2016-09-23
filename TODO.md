### Join operator
This doesn't exist yet. nfu had one but it was awful; this one should support
arbitrary-column joins and not rearrange any columns in the result.

### Document+test dev options
`--explain`, `--dev/parse-one`, etc. Maybe also come up with a better output
format for things like `//options` and `--dev/parse`. Also document why some
options are `--` and others are `//`.

### Throughput monitoring
At the very least we should indicate that data is moving, and where (nfu-style
pipeline monitoring). Need a protocol for this, possibly also a log collector.
Standard error should be piped through the main process, so the pager should be
a fork.

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

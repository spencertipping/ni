### Optimization in general
Rewrite the SHA-1 pure-perl implementation to be something smaller than 30K.

### JSON extractor

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
Maybe a general refactor here. There are some inconsistencies in view rotation,
for example, and the focal point is not always obvious.

#### Client-side axis reordering
Should be trivial since the data is all there.

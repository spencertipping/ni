### Optimized line processor
Right now we're slower than nfu, which is terrible. ni should be able to go
much faster.

### Optimization in general
Rewrite the SHA-1 pure-perl implementation to be something smaller than 30K.

### JSON extractor

### numpy and Octave support
Ideally with binary-read capability, and ideally supporting data facets.

### Binary reads
Simple `pack()`-style reads. This should be fairly trivial to implement.

### Join operator
This doesn't exist yet. nfu had one but it was awful; this one should support
arbitrary-column joins and not rearrange any columns in the result.

### Mnemonic operator refactor
All kinds of confusing stuff happening right now: `@` can be faceting or a
tempfile, `$:` for shell-gen, `::` for SSH gen, `@[]` for ni-gen.

The grammar should be a lot more regular: the metaphor should be "this thing is
an evaluator," implying that the lambda will end up going into a ni process.

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

#### Render fourth dimension (most likely as hue or saturation)
**NB:** this requires that the points be Z-ordered before they're drawn

#### Custom autorange
#### Outlier removal
#### Precise zooming, view history
#### Options to reset/generate view matrix
Maybe a general refactor here. There are some inconsistencies in view rotation,
for example, and the focal point is not always obvious.

#### Client-side axis reordering
Should be trivial since the data is all there.

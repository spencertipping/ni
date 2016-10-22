### Join operator
This doesn't exist yet. nfu had one but it was awful; this one should support
arbitrary-column joins and not rearrange any columns in the result.

### Ops and parsers should be self-documenting

### Document (+test!) cell operators

### Configuration variables should be tied to %ENV
Double-layer namespacing is way too confusing.

### Autoscaling
Like `S`, but auto-configure buffer sizes and #children to maximize throughput.

### Sub-monitors
Right now lambdas don't have monitoring at all, which is lame. We should
monitor lambdas in general, though this requires some cooperation from
operators like `ssh` etc (since we don't yet have a way to traverse lambdas
within op trees).

### Generalized/optimized destructuring
Should apply to JSON, XML, headed CSV/TSV, SQL-as-text, possibly other formats
too. Also should optimize for the consistent-schema case and predict field
positions. Support assertions (?)

### Autofilled directories
e.g. `/mc9drc24` for `/mnt/cache/t9/data/reddit-comments-2015.lz4`

### JSPlot
#### Refactor renderer to support arbitrary operations
#### Labeled axes/grids/etc
Must be client-side; this way it can happen after autoranging and during zooms.

#### View history

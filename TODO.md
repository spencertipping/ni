### Join operator
This doesn't exist yet. nfu had one but it was awful; this one should support
arbitrary-column joins and not rearrange any columns in the result.

### Ops and parsers should be self-documenting

### Autoscaling
Like `S`, but auto-configure buffer sizes and #children to maximize throughput.

### Runs of input files
ni naively appends each input file, but this creates tons of unnecessary
intermediate processes for long file lists. We should preprocess operations to
merge adjacent `cat` invocations (which is easy: `cat` should be a meta-op).

### Sub-monitors
Right now lambdas don't have monitoring at all, which is lame. We should
monitor lambdas in general, though this requires some cooperation from
operators like `ssh` etc (since we don't yet have a way to traverse lambdas
within op trees).

### Generalized/optimized destructuring
Should apply to JSON, XML, headed CSV, SQL-as-text, possibly other formats too.
Also should optimize for the consistent-schema case and predict field
positions.

### Better/more test data
Some data is harder to parse than reddit comments.

### `r` operator should take a lambda
i.e. "take rows for which this lambda streams non-null values"

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

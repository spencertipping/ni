### Fix exit code handling

### Join operator
This doesn't exist yet. nfu had one but it was awful; this one should support
arbitrary-column joins and not rearrange any columns in the result.

#### Inner/outer/left/right 1:1, 1:many, etc joins
#### Fix ordering
The current inner-join operator joins on `\t` to form the comparison key, which
is wrong: it needs to join on `\0` to be consistent with the way `sort` looks
at it. (And I'm not even sure that will work; need to research it more.)

### Scaling operator improvements
#### Autoscale
#### Single-line parallelism (simulate `xargs -n1 -P<N>`)
#### Mergesort when combining streams (thanks @cflaming!)
This is an important one because it means you can scale a pipeline and preserve
data ordering. Mergesorting will be a bottleneck in some cases, but parallelism
could still speed things up considerably.

### Do we want to implement our own sorting function?
There's some possibility it would be faster, and it preemptively avoids issues
around `LC_ALL`.

### Ops and parsers should be self-documenting

### Document (+test!) cell operators
@joycetipping is on it

### Monitors should handle various distribution cases (no clue how)
For example, suppose we have a scale-by-single-row or scale-by-key operator
later on. We might want to know the details of how each of those is working.
This makes me think that fixed space allocation is a non-starter; we might want
some kind of expand/collapse interface, or we might want aggregation by axis.

I think this requires us to write a pager (or at least defer its instantiation)
because `less` captures terminal input immediately.

#### Monitor calculations should be based on first-data moment
If a prior step introduces buffering latency, we shouldn't count this against
the throughput of everything downstream.

### Configuration variables should be tied to %ENV
Double-layer namespacing is way too confusing.

### Better abstraction around procfhs
We need more detailed process tracking, and ideally some more structured
interface to pipelines. Streams should be objects since we aren't
creating/destroying them inside any loops.

### Autoscaling
Like `S`, but auto-configure buffer sizes and #children to maximize throughput.

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

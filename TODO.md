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
Think about whether this is actually necessary; a lot of operators like `HS`
can provide their own auto-looping.

#### Mergesort when combining streams (thanks @cflaming!)
This is an important one because it means you can scale a pipeline and preserve
data ordering. Mergesorting will be a bottleneck in some cases, but parallelism
could still speed things up considerably.

### Do we want to implement our own sorting function?
There's some possibility it would be faster, and it preemptively avoids issues
around `LC_ALL`.

### Autoscaling
Like `S`, but auto-configure buffer sizes and #children to maximize
throughput+latency. Use OO and RMI to customize on the fly.

### JSPlot
#### Enable WebGL rendering for browsers that support it
Also think about using WebGL for realtime query/compute.

[It's possible.](https://gist.github.com/adrianseeley/08ca986403368018c1c3)

#### Buffer on the server and pull down with AJAX
Browsers don't have client-side rate limiting for web sockets, so we can get
situations where it falls over if you stream too much data too quickly.
Server-side buffering also makes it possible to write to disk, etc.

#### Refactor renderer to support arbitrary operations
#### Labeled axes/grids/etc
Must be client-side; this way it can happen after autoranging and during zooms.

#### View history

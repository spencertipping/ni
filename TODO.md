### Optimized line processor
Right now we're slower than nfu, which is terrible. ni should be able to go
much faster.

### Optimization in general
Rewrite the SHA-1 pure-perl implementation to be something smaller than 30K.

### JSON extractor

### `sort` optimizations
`-S`, `--parallel`, etc.

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

### `siproc` needs to capture children more carefully
For example, the output of `ni . \<` is unstable. We also need to capture PIDs
so we can send SIGINT from JSPlot. Otherwise children may run uselessly if they
take a while to emit output.

Use OO for filehandles returned by the stream API. If there are associated
PIDs, then `$fh->kill(INT)` or similar.

### Throughput monitoring
At the very least we should indicate that data is moving, and where (nfu-style
pipeline monitoring). Need a protocol for this, possibly also a log collector.
Standard error should be piped through the main process, so the pager should be
a fork.

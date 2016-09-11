### Optimized line processor
Right now we're slower than nfu, which is terrible. ni should be able to go
much faster.

### Optimization in general
Rewrite the SHA-1 pure-perl implementation to be something smaller than 30K.

### JSON extractor

### Mnemonic operator refactor
All kinds of confusing stuff happening right now: `@` can be faceting or a
tempfile, `$:` for shell-gen, `::` for SSH gen, `@[]` for ni-gen.

### Document+test dev options
`--explain`, `--dev/parse-one`, etc. Maybe also come up with a better output
format for things like `//options` and `--dev/parse`. Also document why some
options are `--` and others are `//`.

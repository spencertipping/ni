# What `src/` does
`src` isn't just a way to organize this repository. `ni-build` creates a ni
image using everything in that directory, so it's good to minimize its size.
(And is why this note is outside of `src/`.)

`ni-build`, via `src/ni/self/fs.sh`, applies some heuristics to image
construction that you'll need to consider before modifying ni's core logic. In
particular, `src/ni/boot.sh` is special and is always loaded first;
`src/ni/meta/*` is loaded next in arbitrary order. After that all remaining
`*.sh` files anywhere in `src/` are evaluated in arbitrary order.

# ni: portable, self-modifying APL for unstructured data
![ni!](http://spencertipping.com/ni.png)

```sh
$ ni --help                     # start here
```

Or start at [doc/README.md](doc/README.md) if you'd prefer to read the docs
online. All of the examples are unit tests, so they should work as advertised;
if they don't, it's likely a system-specific issue and you should definitely
open an issue so I can resolve it.

ni is designed to be extremely portable: it depends only on Perl v5.8.8 or
later, and on POSIX.2001-specified shell utilities like `/bin/sort`. In
practice it means that ni should work out of the box on any system up to a
decade old (and quite possibly older).

## APL, really?
Not literally, but in concept. ni uses a concatenative array language optimized
for brevity and streaming larger-than-memory data. For example, the ubiquitous
big-data word count sorted by descending frequency:

```sh
$ ni README.md FWpF_ CO         # run on POSIX tools locally
$ ni README.md hFWpF_ cO        # run on hadoop streaming
$ ni README.md PL[FWpF_ CO]     # run on local pyspark
```

- `FW`: fieldsplit on non-words
- `pF_`: map with Perl code, in this case `F_`, which is the array of fields
  (this transposes horizontal rows to vertical)
- `C`: sort+count (hadoop version uses `c` instead because the input is already
  sorted)
- `O`: reverse sort numerically

Most operators are a single character, and ni maintains a consistent data
abstraction of "everything's a spreadsheet."

## Dev scripts and testing
- `./build`: create `ni` from source
- `./test`: rebuild and run documentation tests for `ni`

All test cases are extracted from stuff in `doc/` by
[LazyTest](https://github.com/spencertipping/lazytest), and the tests
themselves are run inside various Docker images with different versions of
Perl and other dependencies to test portability.

See [dev/README.md](dev/README.md) for more information about how to hack on
ni.

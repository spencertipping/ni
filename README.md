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

## What everything here does
- `./boot`: create the base `ni` image with no extensions. This image does
  almost nothing and is effectively unusable.
- `./build`: run `./boot`, then extend with a bunch of libraries. See [the dev
  docs](dev/) for information about how this works.
- `core/`: all of the core libraries installed by `./build` onto the base
  image.
- `dev/`: documentation and scripts for ni development.
- `doc/`: documentation for using ni. This ends up being added to the ni image
  by `./build`. Examples in the documentation are extracted into tests by
  `./lazytest`.
- `env/`: dockerfiles for ni portability testing. This way we can make sure it
  works with new and old versions of Perl and coreutils. `./test` loads these.
- `./lazytest`: a copy of
  [LazyTest](https://github.com/spencertipping/lazytest) used to generate unit
  tests from the documentation examples. This is run by `./test`.
- `./ni`: the fully-built ni image if you run `./build`, or just the core image
  if you run `./boot`.
- `src/`: the source files to construct the ni base image.
- `./test`: runs unit tests or launches a test environment. All tests are run
  inside one of the dockerized environments; see the [dev docs](dev/) for more
  details about how this works.

# ni: APL for unstructured streaming data
![ni!](http://spencertipping.com/ni-chroma-rendering.png)

```sh
$ git clone git://github.com/spencertipping/ni && cd ni
$ ./ni --help                   # start here
$ ./ni --js                     # ...or here if you like to live on the edge
```

Some places to start reading:

- [doc/README.md](doc/README.md): really smashing documentation
- [doc/examples.md](doc/examples.md): straight into the deep end with the web
  UI

## For ni developers
- `./boot`: create the base `ni` image with no extensions. This image does
  almost nothing and is effectively unusable.
- `bugs/`: documentation and tests for all nontrivial bugs I've found, starting
  2016.0918.
- `./build`: run `./boot`, then extend with a bunch of libraries. See [the dev
  docs](dev/) for information about how this works.
- `core/`: all of the core libraries installed by `./build` onto the base
  image (which itself is built from `core/boot`).
- `dev/`: documentation and scripts for ni development.
- `doc/`: documentation for using ni. This ends up being added to the ni image
  by `./build`. Examples in the documentation are extracted into tests by
  `./lazytest`. (**Warning:** Turn off your editor's whitespace features if you
  edit `doc/`; test outputs are verified at the byte level, including tabs and
  trailing whitespace in some cases.)
- `env/`: dockerfiles for ni portability testing. This way we can make sure it
  works with new and old versions of Perl and coreutils. `./test` builds and
  runs these.
- `./lazytest`: a copy of
  [LazyTest](https://github.com/spencertipping/lazytest) used to generate unit
  tests from the documentation examples. This is run by `./test`.
- `./ni`: the fully-built ni image if you run `./build`, or just the core image
  if you run `./boot`. The version checked into the repo should always be a
  full build.
- `./test`: runs unit tests or launches a test environment. All tests are run
  inside one of the dockerized environments; see the [dev docs](dev/) for more
  details about how this works.

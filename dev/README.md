# Developing ni
Note that all paths in this README are relative to the repository root, not to
this directory.

## How ni works
See [internals.md](internals.md) for the gory details. For the most part you
won't need to worry about this stuff unless you're writing very involved ni
extensions.

## Building the ni image
```sh
$ ./build                       # constructs ./ni from stuff in src/
```

Internally, this starts by using `./boot` to get a fairly useless `./ni` image
without any libraries in it. It then runs a bunch of commands that look like
this:

```sh
./ni --internal/lib core/common
./ni --internal/lib core/stream
```

Each one of these commands causes `./ni` to integrate the library into its
state and rewrite itself accordingly. [1]

Anytime you write a new core library, you'll need to add an `--internal/lib`
line to `./build` to have it included in the ni image.

## Running tests
```sh
$ ./test                        # runs all tests in all environments
```

You can break the process down into sub-commands if you want to. `./test`
builds a bunch of Docker images from the dockerfiles in [env/](../env/); you
can debug the image build process by doing this:

```sh
$ ./test --build                # just build the images, showing output
$ ./test -b
```

If your tests are failing in a specific image and you want to explore that, you
can get a bash repl:

```sh
$ ./test --repl centos-5        # boot up this test environment
$ ./test --repl                 # boot up ubuntu-16.04
$ ./test -r
```

You can also run the tests in just one environment if you want faster
iteration:

```sh
$ ./test --quick alpine         # run tests in just this environment
$ ./test --quick                # run tests in ubuntu-16.04
$ ./test -q                     # short version
```

And if you're testing a new feature, you can prepend `-o` or `--only` to run
just one markdown file:

```sh
$ ./test --only doc/hadoop.md -q
$ ./test -o doc/hadoop.md -q
```

## Running benchmarks
There are two sets of benchmarks. One is in `dev/hackery/bench`; these are
miscellaneous experiments I ran while considering ways to implement things in
ni, and aren't used to benchmark ni in production.

Production benchmarks are `dev/bench-*`, each of which produces TSV output like
this:

```
ni_perl_256M	21768	ms
ni_perl_r_256M	19214	ms
...
```

These measure the time required to do a specific fixed task. You can run all
benchmarks with `dev/bench`, which will rewrite the benchmark log so you can
track performance by commit.

# Notes
1. The rewriting process itself is implemented by functions in
   [core/boot/self.pl.sdoc](../core/boot/self.pl.sdoc), called by
   `$option_handlers{'internal/lib'}` in
   [core/boot/main.pl.sdoc](../core/boot/main.pl.sdoc). The main function,
   counterintuitively, is called by [core/boot/ni](../core/boot/ni), which is
   the header of the `./ni` script. (ni is a weird program in that it's
   self-modifying and can print its state without reading its source code.)

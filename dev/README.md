# Developing ni
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
line to `./build` to have it included in the ni image. `core/` is just a
symlink to `src/`; it's this way so that ni's internal library names will start
with `core` instead of `src` (yep, really; there's no other reason for the
indirection).

## Running tests
```sh
$ ./test                        # runs all tests in all environments
```

You can break the process down into sub-commands if you want to. `./test`
builds a bunch of Docker images from the dockerfiles in [env/](../env/); you
can debug the image build process by doing this:

```sh
$ ./test --build                # just build the images, showing output
```

If your tests are failing in a specific image and you want to explore that, you
can get a bash repl:

```sh
$ ./test --repl centos-5        # boot up this test environment
$ ./test --repl                 # boot up ubuntu-16.04
```

You can also run the tests in just one environment if you want faster
iteration:

```sh
$ ./test --quick alpine         # run tests in just this environment
$ ./test --quick                # run tests in ubuntu-16.04
```

## Running benchmarks
**TODO**

# Notes
1. The rewriting process itself is implemented by functions in
   [src/self.pl.sdoc](../src/self.pl.sdoc), called by
   `$option_handlers{'internal/lib'}` in
   [src/main.pl.sdoc](../src/main.pl.sdoc). The main function,
   counterintuitively, is called by [src/ni.sdoc](src/ni.sdoc), which is the
   header of the `./ni` script. (ni is a weird program in that it's
   self-modifying and can print its state without reading its source code.)



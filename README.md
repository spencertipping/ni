# ni: APL for unstructured streaming data
![ni!](http://spencertipping.com/ni-chroma-rendering.png)

```sh
$ git clone git://github.com/spencertipping/ni && cd ni
$ ./ni --help                   # start here
$ ./ni --js                     # ...or here if you like to live on the edge
```

Some places to start reading:

- [doc/README.md](doc/README.md): really smashing documentation
- [doc/quickstart.md](doc/quickstart.md): a tutorial that won't say "ni" to you
- [doc/cheatsheet.md](doc/cheatsheet.md): a tutorial that will say "ni" to you
- [doc/examples.md](doc/examples.md): straight into the deep end with the web
  UI

## ni works on every machine
...in the last decade, except Windows. Its only dependency is Perl 5.8 or
later, which is installed basically everywhere (and in any context involving
remote jobs, like hadoop, SSH, docker, etc, ni automatically installs itself so
you don't have to do any configuration).

### Installing ni
```sh
$ git clone git://github.com/spencertipping/ni
$ ln -s $PWD/ni/ni ~/bin/               # if ~/bin is on your $PATH
$ sudo ln -s $PWD/ni/ni /usr/bin/       # system-wide installation
```

## Contributors
- [Factual, Inc](https://github.com/Factual)
- [Joyce Tipping](https://github.com/joycetipping)
- [Michael Bilow](https://github.com/michaelbilow)
- [Spencer Tipping](https://github.com/spencertipping)
- [Wes Henderson](https://github.com/weshenderson)

## License
(Also included in the top of the ni image, and available by running `ni
//license`.)

```
Copyright (c) 2016 Spencer Tipping

MIT license

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## For ni developers
### [Amazingly cool development documentation](dev/README.md)

### How to test your changes
```sh
$ ./test                        # accurate but time-consuming; see below
```

`./test` automatically rebuilds your `./ni` image from source.

If you've just made a small change and don't want to wait the full ten minutes
it takes to run all tests on all distributions, here's a lighter-weight way to
do it:

```sh
$ ./test --build                # if you're testing for the first time
$ ./test --quick centos-5       # faster than running all tests
$ ./test --quick ubuntu-16.04   # includes hadoop/spark/docker tests
```

### Main files and what they do
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

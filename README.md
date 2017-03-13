# ni: bash+Smalltalk+APL+Forth for data science
![ni!](http://spencertipping.com/ni-chroma-rendering.png)

## Installation
```sh
$ git clone git://github.com/spencertipping/ni && ln -s $PWD/ni/ni ~/bin/
```

ni works on every machine in the last decade, except Windows. Its only
dependency is Perl 5.8 or later, which is installed basically everywhere (and
in any context involving remote jobs, like hadoop, SSH, docker, etc, ni
automatically installs itself so you don't have to do any configuration).

## Docs
- [Ni by Example](doc/ni_by_example_1.md): a tutorial that won't say "ni" to you
- [The Cheatsheet](doc/cheatsheet.md): a tutorial that will say "ni" to you
- [Web UI Examples](doc/examples.md): straight into the deep end with the web UI

ni is also self-documenting; see `ni --help`.

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
Copyright (c) 2016-2017 Spencer Tipping

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
### How to test your changes
```sh
$ ./test --docker               # tests ni in a bunch of environments
$ less test.log                 # see what went wrong, if anything
```

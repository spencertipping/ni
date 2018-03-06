<h1 align="center">
<br>
<a href="https://github.com/spencertipping/ni"><img src="http://spencertipping.com/ni-logo.png" alt="ni"></a>
<br>
ni is two-fisted data science.
<br>
<br>
<a href='https://travis-ci.org/spencertipping/ni'><img src='https://travis-ci.org/spencertipping/ni.svg?branch=develop' alt='Travis CI'></a>
<br>
<img src='http://spencertipping.com/ni-osm.gif'>
</h1>

## Features
- [Zero-dependency installation: clone the repo and you're done](#getting-started)
- [Automatic self-installation over SSH for remote pipeline execution](doc/net.md)
- [Automatic self-installation onto Hadoop worker nodes](doc/hadoop.md)
- [Automatic self-installation into Docker containers](doc/container.md)
- [Self-contained and extensible](doc/libraries.md)
- [Streaming, constant-space data processing](doc/stream.md)
- [Concise syntax for unstructured data transformation](doc/ni_fu.md)
  - Highly stable API: unit tests are generated from documentation examples
- [Realtime preview, throughput, and bottleneck monitoring](doc/monitor.md)
- [Horizontal pipeline scaling to parallelize slow pipeline regions](doc/scale.md)
- [Realtime 2D/3D visualization of arbitrarily large datasets](https://github.com/spencertipping/www/blob/master/audio.md)
  - Builtin HTTP/websocket server for offline data visualization

### Interoperability with common data formats
- [Compressed data](https://github.com/spencertipping/osm#openstreetmap-data-processing)
  - Automatic detection + decompression based on stream contents
- [Date/time](doc/ni_by_example_3.md#time-perl-functions)
- [JSON](doc/ni_by_example_3.md#json-io)
- [Binary files](doc/binary.md)
- [Bloom filters](doc/bloom.md)
- [Geohashes](doc/ni_by_example_3.md#geographic-perl-functions)
- [NumPy](doc/matrix.md#numpy-interop)

## Getting started
```sh
$ git clone git://github.com/spencertipping/ni
$ sudo ln -s $PWD/ni/ni /usr/bin/
```

Now you have ni installed; you can try it out like this:

```sh
$ ni n10
```

**Use `ni` anywhere you would normally use `less`.** ni will automatically
decompress streams, and it supports a variety of data sources:

```sh
$ ni https://google.com
$ ni /etc/passwd
$ find . | ni
$ echo "hi" | bzip2 | ni
```

ni will intermittently print monitor updates that may overwrite your output; use
`Ctrl+L` twice to refresh the screen.

<h2 align='center'>
<img alt='ni explain' src='http://spencertipping.com/ni-explain.png'>
</h2>

![ni basics](http://spencertipping.com/ni-basics.gif)

### Ni By Example
- [Chapter 1: Streams](doc/ni_by_example_1.md)
- [Chapter 2: Perl scripting](doc/ni_by_example_2.md)
- [Chapter 3: ni's Perl API, JSON, datetime](doc/ni_by_example_3.md)
- [Chapter 4: Data closures, Hadoop](doc/ni_by_example_4.md)
- [Chapter 5: Jupyter interop, matrix operations, joins](doc/ni_by_example_5.md)
- [ni fu](doc/ni_fu.md)
- [Operator cheatsheet](doc/cheatsheet_op.md)
- [Perl cheatsheet](doc/cheatsheet_perl.md)

<h2 align='center'>
<img alt='ni license' src='http://spencertipping.com/ni-license.png'>
</h2>

**MIT license**

Copyright (c) 2016-2018 Spencer Tipping

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

### Contributors
- [Factual, Inc](https://github.com/Factual)
- [Joyce Tipping](https://github.com/joycetipping)
- [Michael Bilow](https://github.com/michaelbilow)
- [Spencer Tipping](https://github.com/spencertipping)
- [Wes Henderson](https://github.com/weshenderson)

<h1 align="center">
<br>
<a href="https://github.com/spencertipping/ni"><img src="http://spencertipping.com/ni-logo.png" alt="ni"></a>
<br>
ni: two-fisted data science
<br>
<img src='https://travis-ci.org/spencertipping/ni.svg?branch=develop' alt='Travis CI'>
<br>
</h1>

<h3 align="center">
A self-contained command line tool for quickly transforming and visualizing data
of any size.
</h3>

## Features
- [Zero-dependency installation: clone the repo and you're done](#getting-started)
  - ni's only dependency is core Perl 5, which is installed nearly everywhere
- Portability across OSX, Linux, and other POSIX operating systems
  - Automatic self-installation over SSH for remote pipeline execution
  - Automatic self-installation onto Hadoop worker nodes
  - Runs on PCs, Raspberry Pi, and Android phones (haven't tested iOS yet)
- Streaming, constant-space data processing
- Concise syntax for unstructured data transformation
- [Realtime preview, throughput, and bottleneck monitoring](doc/monitor.md)
  - [Horizontal pipeline scaling to parallelize slow pipeline regions](doc/scale.md)
- Realtime 2D/3D visualization of arbitrarily large datasets

## Getting started
```sh
$ git clone git://github.com/spencertipping/ni
$ sudo ln -s $PWD/ni/ni /usr/bin/
```

<h2 align='center'>
<img src='http://spencertipping.com/ni-explain.png'>
<br>
Using ni
</h2>



## Contributors
- [Factual, Inc](https://github.com/Factual)
- [Joyce Tipping](https://github.com/joycetipping)
- [Michael Bilow](https://github.com/michaelbilow)
- [Spencer Tipping](https://github.com/spencertipping)
- [Wes Henderson](https://github.com/weshenderson)

## License
```
Copyright (c) 2016-2018 Spencer Tipping

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

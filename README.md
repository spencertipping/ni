<h1 align="center">
<br>
<a href="https://github.com/spencertipping/ni"><img src="http://spencertipping.com/ni-logo.png" alt="ni"></a>
<br>
ni is two-fisted data science. <a href='https://travis-ci.org/spencertipping/ni'><img src='https://travis-ci.org/spencertipping/ni.svg?branch=develop' alt='Travis CI'></a>
<br>
<br>
<br>
<img src='http://spencertipping.com/ni-osm.gif'>
</h1>

## Support
- For ni usage: [#ni on dev.spencertipping.com](https://dev.spencertipping.com/channel/ni)
- For ni developers: [#ni-dev on dev.spencertipping.com](https://dev.spencertipping.com/channel/ni-dev)

## Features
- [Zero-dependency installation: clone the repo and you're done](#getting-started)
- [Launch Hadoop Streaming jobs with five characters](doc/ni_by_example_4.md#hadoop-streaming-mapreduce)
- [Automatic self-installation over SSH for remote pipeline execution](doc/net.md)
- [Automatic self-installation into Docker containers](doc/container.md)
- [Self-contained and extensible](doc/libraries.md)
- [Data closures: lambda capture for data pipelines](doc/closure.md)
- [Streaming, constant-space data processing](doc/stream.md)
- [Concise syntax for unstructured data transformation](doc/ni_fu.md)
  - Highly stable API: unit tests are generated from documentation examples
- [Realtime preview, throughput, and bottleneck monitoring](doc/monitor.md)
- [Horizontal pipeline scaling to parallelize slow pipeline regions](doc/scale.md)
- [Realtime 2D/3D visualization of arbitrarily large datasets](https://github.com/spencertipping/www/blob/master/audio.md)
  - Builtin HTTP/websocket server for offline data visualization
- [Online documentation and source code introspection](#online-documentation)

![image](http://storage6.static.itmages.com/i/18/0306/h_1520341324_1461936_f6fd3073ba.png)

### Interoperability
- [Compressed data](https://github.com/spencertipping/osm#openstreetmap-data-processing)
  - Automatic detection + decompression based on stream contents
- [Date/time](doc/ni_by_example_3.md#time-perl-functions)
- [JSON](doc/ni_by_example_3.md#json-io)
- [Binary files](doc/binary.md)
- [Bloom filters](doc/bloom.md)
- [Geohashes](doc/ni_by_example_3.md#geographic-perl-functions)
- [NumPy](doc/matrix.md#numpy-interop)
- [Git](doc/git.md)
- [MapBox, GeoJSON, and WKT](doc/wkt.md)

## Getting started
```sh
$ git clone git://github.com/spencertipping/ni
$ sudo ln -s $PWD/ni/ni /usr/bin/
```

So ... what is ni?

![ni basics](http://spencertipping.com/ni-basics.gif)

### `ni` is `cat` and `less` (and `zless`, `bzless`, etc)
```sh
$ ni /etc/passwd
$ ni /usr/share/dict/words
$ ni /usr/share/man/man1/ls.1.gz
$ find . | ni
$ echo hi | bzip2 | ni                  # auto-decompression
```

### `ni` is `gzip -dc`, `xz -dc`, `lz4 -dc`, etc
ni knows the magic number for common compression formats and invokes the correct
decompressor automatically.

```sh
$ cat mystery-file | ni > decoded-file
```

### `ni` is `pv`/`pipemeter`
```sh
$ find / | ni > /dev/null               # == cat, but show data throughput
```

(**NB:** if you're not redirecting data to `/dev/null` or a file, ni may
intermittently print monitor updates that temporarily overwrite your output; use
`Ctrl+L` twice to refresh the screen.)

### `ni` is `ls`
...but often faster because it doesn't look at file attributes; it just gives
you the listing.

```sh
$ ni /
$ ni /etc
$ ni .
```

### `ni` is `curl`
```sh
$ ni https://google.com
$ ni http://wikipedia.org http://github.com
```

### `ni` is `seq`
```sh
$ ni n100
$ ni n01000
$ ni nE6                                # E6 == 10^6 = 1000000
```

### `ni` is `grep`
ni's `r//` operator searches for rows which match a regular expression:

```sh
$ ni n1000 | ni r/77/
```

### `ni` is `|`
In general, `ni X Y` == `ni X | ni Y`. Data generators like files are appended
to the stream: `ni /etc/passwd` == `cat - /etc/passwd`.

```sh
$ ni n1000 r/77/
$ ni n1000 r/77/ r/3/
```

### `ni` is `echo`
```sh
$ ni ifoo                               # == echo foo
$ ni i[foo bar]                         # == echo -e "foo\tbar"
```

### `ni` is `xargs ni` (`xargs cat`)
```sh
$ ni /etc \<                            # \< == xargs ni, give or take
$ ni /usr/share/man/man1 \<             # \< auto-decompresses files
$ ni ihttps://google.com /etc \<        # \< recognizes URL formats
```

### `ni` is `hadoop fs -cat` and `hadoop fs -text`
```sh
$ ni hdfs:///path/to/file               # == hadoop fs -cat /path/to/file
$ ni hdfst:///path/to/file              # == hadoop fs -text /path/to/file
```

### `ni` is `unzip` and `tar -x`, but better
```sh
$ ni tar://myfile.tgz                   # == tar -tzf myfile.tgz
$ ni zip://myfile.zip                   # == zip file listing
$ ni tarentry://myfile.tgz:foo.txt      # contents of specific tar entry
$ ni zipentry://myfile.zip:foo.txt      # contents of specific zip entry
```

### `ni` reads `xlsx`
```sh
$ ni xlsx://spreadsheet.xlsx            # list of sheets
$ ni xlsxsheet://spreadsheet.xlsx:1     # contents of sheet 1 as TSV
```

### `ni` is `xargs -P` for data
```sh
$ find /usr -type f \
    | ni \< S4[ r'/all your base/' ]    # use four workers for r// operator
```

### `ni` is `ssh`
...and self-installs on remote hosts.

```sh
$ ni shost[ /etc/hostname ]             # == ssh host ni /etc/hostname | ni
```

### `ni` is a lot more
Ni By Example, courtesy of [Michael Bilow](https://github.com/michaelbilow):

- [Chapter 1: Streams](doc/ni_by_example_1.md)
- [Chapter 2: Perl scripting](doc/ni_by_example_2.md)
- [Chapter 3: ni's Perl API, JSON, datetime](doc/ni_by_example_3.md)
- [Chapter 4: Data closures, Hadoop](doc/ni_by_example_4.md)
- [Chapter 5: Jupyter interop, matrix operations, joins](doc/ni_by_example_5.md)
- [ni fu](doc/ni_fu.md)
- [Operator cheatsheet](doc/cheatsheet_op.md)
- [Perl cheatsheet](doc/cheatsheet_perl.md)

<h2 align='center'>
<img alt='ni explain' src='http://spencertipping.com/ni-explain.png'>
</h2>

### Online documentation
![ni inspect](http://spencertipping.com/ni-inspect.gif)

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

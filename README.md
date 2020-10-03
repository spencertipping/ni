<h1 align="center">
<br>
<a href="https://github.com/spencertipping/ni"><img src="https://spencertipping.com/ni-logo.png" alt="ni"></a>
<br>
ni says "ni" to your data. <a href='https://travis-ci.org/spencertipping/ni'><img src='https://travis-ci.org/spencertipping/ni.svg?branch=develop' alt='Travis CI'></a>
<br>
<br>
<br>
<img src='https://spencertipping.com/ni-osm.gif'>
</h1>


## Installing `ni`
```sh
$ git clone git://github.com/spencertipping/ni
$ sudo ln -s $PWD/ni/ni /usr/bin/
```

You only need to install `ni` on the machine you're using. `ni` will install
itself in memory on machines you point it at, e.g. by using `ssh` or Hadoop to
move sections of pipelines.


## What is `ni`?
`ni` is a way to write data processing pipelines in bash. It prioritizes
brevity, low latency, high throughput, portability, and ease of iteration.
Concretely, `ni` is a Perl script with a sophisticated grammar that builds shell
pipelines from UNIX builtins (e.g. `perl`, `sort`, `gzip`, `ssh`, `ffmpeg`). You
could think of it as a macro layer for bash, plus some logic to handle job
distribution and horizontal scaling.

Here's an example workflow to look at attempted SSH logins in
`/var/log/auth.log`:

![ni basics](https://spencertipping.com/ni-basics.gif)


### `ni` is efficient for big and small data
`ni` can process terabytes or petabytes of data in constant space, and knows
about things like GNU `sort`'s `--compress-program` option to make it possible
to process more data than will fit on disk. It can interoperate with Hadoop and
self-install on workers if you have a cluster available. Commands written in
`ni` are typically as fast or faster than hand-written equivalents.

`ni` can process full datasets on one machine, e.g.
[Wikipedia (~40GB)](https://en.wikipedia.org/wiki/Wikipedia:Database_download),
[OpenStreetMap (~400GB)](https://github.com/spencertipping/www/blob/master/osm-animation.md), and
[Reddit (~1.5TB)](https://files.pushshift.io/reddit/). Intermediate streams
aren't written to disk unless you sort them.


### `ni` is [`cat` and `less` (and `zless`, `bzless`, etc)](doc/ni_by_example_1.md#file-input)
```sh
$ ni /etc/passwd
$ ni /usr/share/dict/words
$ ni /usr/share/man/man1/ls.1.gz
$ find . | ni
```


### `ni` is [`gzip -dc`, `xz -dc`, `lz4 -dc`, etc](doc/ni_by_example_1.md#z-compression)
ni knows the magic number for common compression formats and invokes the correct
decompressor automatically if you have it installed.

```sh
$ cat mystery-file | ni > decoded-file
```


### `ni` is [`pv`/`pipemeter`](doc/monitor.md)
```sh
$ find / | ni > /dev/null               # == cat, but show data throughput
```

(**NB:** if you're not redirecting data to `/dev/null` or a file, ni may
intermittently print monitor updates that temporarily overwrite your output; use
`Ctrl+L` twice to refresh the screen.)


### `ni` is [`ls`](doc/ni_by_example_1.md#directory-io)
...but often faster because it doesn't look at file attributes; it just gives
you the listing.

```sh
$ ni /
$ ni /etc
$ ni .
```


### `ni` is [`curl`/`sftp`](doc/ni_by_example_3.md#https-http-sftp-s3cmd-read-from-web-sources)
```sh
$ ni https://google.com
$ ni http://wikipedia.org http://github.com
```


### `ni` is [`seq`](doc/ni_by_example_1.md#n-integer-stream)
```sh
$ ni n100
$ ni n01000
$ ni nE6                                # E6 == 10^6 = 1000000
```


### `ni` is [`grep`](doc/ni_by_example_1.md#filtering-with-r)
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


### `ni` is [`echo`](doc/ni_by_example_1.md#i-literal-text)
```sh
$ ni ifoo                               # == echo foo
$ ni i[foo bar]                         # == echo -e "foo\tbar"
```


### `ni` is [`xargs ni` (`xargs cat`)](doc/ni_by_example_1.md#-read-from-filenames)
```sh
$ ni /etc \<                            # \< == xargs ni, give or take
$ ni /usr/share/man/man1 \<             # \< auto-decompresses files
$ ni ihttps://google.com /etc \<        # \< recognizes URL formats
```


### `ni` is [`hadoop fs -cat` and `hadoop fs -text`](doc/ni_by_example_4.md#hdfs-io)
```sh
$ ni hdfs:///path/to/file               # == hadoop fs -cat /path/to/file
$ ni hdfst:///path/to/file              # == hadoop fs -text /path/to/file
```

ni can also run Hadoop Streaming jobs with itself nondestructively installed on
worker nodes.


### `ni` is [`git ls-tree` etc](doc/git.md)
```sh
$ ni git://.                            # show all branches/tags
$ ni githistory://.:develop             # full history of develop branch
$ ni githistory://.:develop::a/file     # full history of a file on develop
$ ni gittree://.:develop                # file listing for develop branch
$ ni gittree://.:develop::folder        # directory listing at develop revision
$ ni gitsnap://.:master^                # all blobs one commit before master
$ ni gitblob://.:18891afd4              # file contents
$ ni gitblob://.:develop::ni            # file contents of 'ni' on develop
$ ni gitdiff://.:master..develop        # regular diff
$ ni gitpdiff://.:develop               # processed diff
$ ni gitpdiff://.:develop::path/path    # processed diff for a specific path
```


### `ni` wraps `sqlite3`
```sh
$ ni sqlite:///path/to/file.db          # list tables in database
$ ni sqlitet:///path/to/file.db:table   # output all table data as TSV
$ ni sqlites:///path/to/file.db:table   # output table schema as SQL
$ ni sqliteq:///path/to/file.db:'sql'   # output SQL results as TSV
```


### `ni` is a Wikipedia reader
```sh
$ ni wiki://JPEG      # English-language, download MediaWiki source
$ ni enws://JPEG      # same thing
$ ni enwt://JPEG      # English-language, download as text
$ ni simplewt://JPEG  # Simple Wikipedia, download as text
```


### `ni` plays streaming audio and video (using `ffplay`)
```sh
$ ni video.mp4 VP
$ ni https://website.com/some-music.ogg VP
$ ni m3u://file.m3u VP                            # stream from m3u file
$ ni m3u://https://website/m3u
$ ni m3u+webm://https://website/m3u > video.webm  # specify container format
```


### `ni` wraps `ffmpeg` and `youtube-dl`
```sh
$ ni yt://dQw4w9WgXcQ VP                # watch a youtube video browserless
$ ni yt://dQw4w9WgXcQ AEogg/libvorbis/224k > audio.ogg
$ ni yt://dQw4w9WgXcQ VIpng > pngstream # concatenated PNG frames of video
$ ni yt://dQw4w9WgXcQ VImjpeg IVavi VP  # vid->jpg then jpg->avi
```


### `ni` is [`unzip` and `tar -x/-t`, but better](doc/ni_by_example_3.md#compressed-archive-input)
```sh
$ ni tar://myfile.tgz                   # == tar -tzf myfile.tgz (requires tar)
$ ni zip://myfile.zip                   # == zip file listing (requires unzip)
$ ni 7z://myfile.7z                     # == 7zip file listing (requires 7z, 7za, 7zr, or p7zip)
$ ni tarentry://myfile.tgz:foo.txt      # contents of specific tar entry
$ ni zipentry://myfile.zip:foo.txt      # contents of specific zip entry
$ ni 7zentry://myfile.7z:foo.txt        # contents of specific 7zip entry
```


### `ni` reads `xlsx` (sometimes)
```sh
$ ni xlsx://spreadsheet.xlsx            # list of sheets
$ ni xlsxsheet://spreadsheet.xlsx:1     # contents of sheet 1 as TSV
```


### `ni` is [`xargs -P` for data](doc/ni_by_example_4.md#s-horizontal-scaling)
```sh
$ find /usr -type f \
    | ni \< S4[ r'/all your base/' ]    # use four workers for r// operator
```


### `ni` is [`ssh`](doc/ni_by_example_4.md#ssh-containers-and-horizontal-scaling)
...and nondestructively self-installs on remote hosts.

```sh
$ ni shost[ /etc/hostname ]             # == ssh host ni /etc/hostname | ni
```


### `ni` is interoperable
- [Date/time](doc/ni_by_example_3.md#time-perl-functions)
- [JSON](doc/ni_by_example_3.md#json-io)
- [Binary files](doc/binary.md)
- [Bloom filters](doc/bloom.md)
- [Geohashes](doc/ni_by_example_3.md#geographic-perl-functions)
- [NumPy](doc/matrix.md#numpy-interop)
- [Git](doc/git.md)
- [MapBox, GeoJSON, and WKT](doc/wkt.md)


### `ni` is [realtime visualization for big data](doc/visual.md)
```sh
$ ni --js                               # start the webserver (Ctrl+C to exit)
http://localhost:8090                   # open this link in a browser
```

![image](https://spencertipping.com/niwav.gif)


<h2 align='center'>
<img alt='ni explain' src='https://spencertipping.com/ni-explain.png'>
</h2>

## RocketChat support forums
- ni usage: [#ni on dev.spencertipping.com](https://dev.spencertipping.com/channel/ni)
- general data science: [#datascience on dev.spencertipping.com](https://dev.spencertipping.com/channel/datascience)
- ni development: [#ni-dev on dev.spencertipping.com](https://dev.spencertipping.com/channel/ni-dev)


## Ni By Example
An excellent guide by [Michael Bilow](https://github.com/michaelbilow):

- [Chapter 1: Streams](doc/ni_by_example_1.md)
- [Chapter 2: Perl scripting](doc/ni_by_example_2.md)
- [Chapter 3: ni's Perl API, JSON, datetime](doc/ni_by_example_3.md)
- [Chapter 4: Data closures, Hadoop](doc/ni_by_example_4.md)
- [Chapter 5: Jupyter interop, matrix operations, joins](doc/ni_by_example_5.md)
- [Chapter 6: More functions, advanced Perl (WIP)](doc/ni_by_example_6.md)
- [ni fu](doc/ni_fu.md)
- [Operator cheatsheet](doc/cheatsheet_op.md)
- [Perl cheatsheet](doc/cheatsheet_perl.md)


<h2 align='center'>
<img alt='ni license' src='https://spencertipping.com/ni-license.png'>
</h2>

**MIT license**

Copyright (c) 2016-2020 Spencer Tipping

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

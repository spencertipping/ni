# ni tutorial
You can access this tutorial by running `ni //help` or `ni //help/tutorial`. See
also `ni //usage`, or run `ni` without options.

ni parses its command arguments to build and run a data pipeline. You can get
started by using it like a version of `less` that knows how to decompress
things, but it can do a lot more; see the topics below.

```sh
$ ni data-source.gz                     # works like less
$ cat data-source.gz | ni               # same here
$ ni //help/stream                      # view a help topic
```

## The deep end
- [examples.md](examples.md) (`ni //help/examples`)

## Essentials
- [stream.md](stream.md)   (`ni //help/stream`):  intro to ni grammar and data
- [row.md](row.md)         (`ni //help/row`):     row-level operators
- [col.md](col.md)         (`ni //help/col`):     column-level operators
- [perl.md](perl.md)       (`ni //help/perl`):    ni's Perl library
- [lisp.md](lisp.md)       (`ni //help/lisp`):    ni's Common Lisp library
- [ruby.md](ruby.md)       (`ni //help/ruby`):    ni's Ruby library
- [monitor.md](monitor.md) (`ni //help/monitor`): pipeline monitoring

## Stuff worth knowing about
- [net.md](net.md)             (`ni //help/net`):       HTTP/SSH/etc
- [scale.md](scale.md)         (`ni //help/scale`):     parallelizing stuff
- [closure.md](closure.md)     (`ni //help/closure`):   data closures
- [hadoop.md](hadoop.md)       (`ni //help/hadoop`);    Hadoop interop
- [visual.md](visual.md)       (`ni //help/visual`):    visualizing data
- [matrix.md](matrix.md)       (`ni //help/matrix`):    dense/sparse matrices
- [binary.md](binary.md)       (`ni //help/binary`):    decoding binary streams
- [container.md](container.md) (`ni //help/container`): Dockerizing stuff
- [json.md](json.md)           (`ni //help/json`):      working with JSON
- [warnings.md](warnings.md)   (`ni //help/warnings`):  things to look out for

## Reference
- [options.md](options.md) (`ni //help/options`): every CLI option and
  operator, each with example usage

## Extending ni
- [extend.md](extend.md)       (`ni //help/extend`):    how to write a ni
  extension
- [libraries.md](libraries.md) (`ni //help/libraries`): how to load/use a
  library

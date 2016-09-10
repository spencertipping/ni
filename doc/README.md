# ni tutorial
You can access this tutorial by running `ni //help` or `ni //help/tutorial`.

ni parses its command arguments to build and run a data pipeline. You can get
started by using it like a version of `less` that knows how to decompress
things, but it can do a lot more; see the topics below.

```sh
$ ni data-source.gz                     # works like less
$ cat data-source.gz | ni               # same here
$ ni //help/stream                      # view a help topic
```

## Basics
- [stream.md](stream.md) (`ni //help/stream`): intro to ni grammar and data
- [row.md](row.md)       (`ni //help/row`):    row-level operators
- [col.md](col.md)       (`ni //help/col`):    column-level operators
- [perl.md](perl.md)     (`ni //help/perl`):   ni's Perl library
- [lisp.md](lisp.md)     (`ni //help/lisp`):   ni's Common Lisp library
- [ruby.md](ruby.md)     (`ni //help/ruby`):   ni's Ruby library
- [facet.md](facet.md)   (`ni //help/facet`):  the faceting operator
- [visual.md](visual.md) (`ni //help/visual`): visualizing data

## Reference
You can use `ni //options` to get a list of all parsing rules ni applies to the
command line. The output format is a TSV of `context long/short name parser`.

- [options.md](options.md) (`ni //help/options`): every CLI option and
  operator, each with example usage

## Extending ni
- [extend.md](extend.md)       (`ni //help/extend`):    how to write a ni
  extension
- [libraries.md](libraries.md) (`ni //help/libraries`): how to load/use a
  library

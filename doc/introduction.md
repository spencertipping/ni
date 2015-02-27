# Introduction
Without options, `ni` works just like `less` or `cat`:

```sh
$ echo hi | ni
hi
$ echo foo > bar
$ ni bar
foo
```

You can also combine files and refer to compressed data; `ni` knows how to
decompress a bunch of common formats:

```sh
$ echo hi from gzip | gzip > hi.gz
$ echo hi from bzip2 | bzip2 > hi.bz2
$ echo hi from lzop | lzop > hi.lzo
$ echo hi from xz | xz > hi.xz
$ ni hi.gz hi.bz2 hi.lzo hi.xz
hi from gzip
hi from bzip2
hi from lzop
hi from xz
```

It wouldn't be very interesting if that's all ni did, however, so let's talk
about some stream manipulation commands.

## Row mapping
```sh
$ seq 10 | ni -m '%0, %0 * %0'
1	1
2	4
3	9
4	16
5	25
6	36
7	49
8	64
9	81
10	100
```

`%0` is a reference to column 0, which contains the numbers from `seq`. The
command above can be read as, "for each number from 1 to 10, produce the number
followed by its square" -- and when written to a file or the terminal, ni
separates columns using tabs.

```sh
$ seq 4
1
2
3
4
$ ni --iota 4
0
1
2
3
$ ni -i4 -m '%0, %0 * %0'
0	0
1	1
2	4
3	9
$ ni -i4m '%0, %0 * %0'
0	0
1	1
2	4
3	9
```

## Field selection
```sh
$ ni -i4m 'map %0 ** $_, 1 .. 5' -f12
0	0
1	1
4	8
9	27
```

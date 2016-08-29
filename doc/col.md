# Column operations
ni models incoming data as a tab-delimited spreadsheet and provides some
operators that allow you to manipulate the columns in a stream accordingly. The
two important ones are `f[columns...]` to rearrange columns, and `F[delimiter]`
to create new ones.

ni always refers to columns using letters: `A` to `Z`.

## Reordering
First let's generate some data, in this case an 8x8 multiplication table:

```bash
$ ni n:8p'r map a*$_, 1..8' > mult-table
$ ni mult-table
1	2	3	4	5	6	7	8
2	4	6	8	10	12	14	16
3	6	9	12	15	18	21	24
4	8	12	16	20	24	28	32
5	10	15	20	25	30	35	40
6	12	18	24	30	36	42	48
7	14	21	28	35	42	49	56
8	16	24	32	40	48	56	64
```

The `f` operator takes a multi-column spec and reorders, duplicates, or deletes
columns accordingly.

```bash
$ ni mult-table fA      # the first column
1
2
3
4
5
6
7
8
$ ni mult-table fDC     # fourth, then third column
4	3
8	6
12	9
16	12
20	15
24	18
28	21
32	24
$ ni mult-table fAA     # first column, duplicated
1	1
2	2
3	3
4	4
5	5
6	6
7	7
8	8
```

You can also choose "the rest of the columns" using `.` within your column
spec. This selects everything to the right of the rightmost column you've
mentioned.

```bash
$ ni mult-table fDA.    # fourth, first, "and the rest (i.e. 5-8)"
4	1	5	6	7	8
8	2	10	12	14	16
12	3	15	18	21	24
16	4	20	24	28	32
20	5	25	30	35	40
24	6	30	36	42	48
28	7	35	42	49	56
32	8	40	48	56	64
$ ni mult-table fBA.    # an easy way to swap first two columns
2	1	3	4	5	6	7	8
4	2	6	8	10	12	14	16
6	3	9	12	15	18	21	24
8	4	12	16	20	24	28	32
10	5	15	20	25	30	35	40
12	6	18	24	30	36	42	48
14	7	21	28	35	42	49	56
16	8	24	32	40	48	56	64
```

## Splitting
The `F` operator gives you a way to convert non-tab-delimited data into TSV.
For example, if you're parsing `/etc/passwd`, you'd turn colons into tabs
first.

```bash
$ ni /etc/passwd r2F/:/
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
```

`F` has the following uses:

- `F/regex/`: split on occurrences of regex. If present, the first capture
  group will be included before a tab is appended to a field.
- `Fm/regex/`: don't split; instead, look for matches of regex and use those as
  the field values.
- `FC`: split on commas
- `FS`: split on runs of horizontal whitespace
- `FW`: split on runs of non-word characters
- `FP`: split on pipe symbols

Note that `FC` isn't a proper CSV parser; it just transliterates all commas
into tabs.

```bash
$ ni //ni r3                            # some data
#!/usr/bin/env perl
# ni: https://github.com/spencertipping/ni
# Copyright (c) 2016 Spencer Tipping
```

```bash
$ ni //ni r3F/\\//                      # split on forward slashes
#!	usr	bin	env perl
# ni: https:		github.com	spencertipping	ni
# Copyright (c) 2016 Spencer Tipping
```

```bash
$ ni //ni r3FW                          # split on non-words
	usr	bin	env	perl
	ni	https	github	com	spencertipping	ni
	Copyright	c	2016	Spencer	Tipping
```

```bash
$ ni //ni r3FS                          # split on whitespace
#!/usr/bin/env	perl
#	ni:	https://github.com/spencertipping/ni
#	Copyright	(c)	2016	Spencer	Tipping
```

```bash
$ ni //ni r3Fm'/\/\w+/'                 # words beginning with a slash
/usr	/bin	/env
/github	/spencertipping	/ni

```

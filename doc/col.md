# Column operations
ni models incoming data as a tab-delimited spreadsheet and provides some
operators that allow you to manipulate the columns in a stream accordingly. The
two important ones are `f[columns...]` to rearrange columns, and `F[delimiter]`
to create new ones.

ni refers to columns using letters: `A` to `Z`, though you can also use the
form `#N` to address the Nth column, where `#0` == `A` and `#25` == `Z`. This
second form can be useful for large data files.

## Reordering
First let's generate some data, in this case an 8x8 multiplication table:

```bash
$ ni n8p'r map a*$_, 1..8' > mult-table
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
$ ni mult-table fA-D    # first four columns
1	2	3	4
2	4	6	8
3	6	9	12
4	8	12	16
5	10	15	20
6	12	18	24
7	14	21	28
8	16	24	32
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
$ ni mult-table x       # even easier (see below)
2	1	3	4	5	6	7	8
4	2	6	8	10	12	14	16
6	3	9	12	15	18	21	24
8	4	12	16	20	24	28	32
10	5	15	20	25	30	35	40
12	6	18	24	30	36	42	48
14	7	21	28	35	42	49	56
16	8	24	32	40	48	56	64
```

## Exchanging
You can swap columns into leading positions using the `x` operator:

```bash
$ ni mult-table xC r2   # swap third column into first position
3	2	1	4	5	6	7	8
6	4	2	8	10	12	14	16
$ ni mult-table xGHr2   # swap seventh, eighth columns into first two
7	8	3	4	5	6	1	2
14	16	6	8	10	12	2	4
$ ni mult-table xr2     # swap first two columns
2	1	3	4	5	6	7	8
4	2	6	8	10	12	14	16
```

## Splitting
The `F` operator gives you a way to convert non-tab-delimited data into TSV.
For example, if you're parsing `/etc/passwd`, you'd turn colons into tabs
first.

`F` has the following uses:

- `F:<char>`: split on character
- `F/regex/`: split on occurrences of regex. If present, the first capture
  group will be included before a tab is appended to a field.
- `Fm/regex/`: don't split; instead, look for matches of regex and use those as
  the field values.
- `FC`: split on commas (doesn't handle special CSV cases)
- `FV`: parse CSV "correctly", up to newlines in fields
- `FS`: split on runs of horizontal whitespace
- `FW`: split on runs of non-word characters
- `FP`: split on pipe symbols

### Examples
```bash
$ ni /etc/passwd r2F::          # F: followed by :, which is the split char
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
```

```bash
$ ni //ni r3                            # some data
#!/usr/bin/env perl
$ni::self{license} = <<'_';
ni: https://github.com/spencertipping/ni
```

```bash
$ ni //ni r3F/\\//                      # split on forward slashes
#!	usr	bin	env perl
$ni::self{license} = <<'_';
ni: https:		github.com	spencertipping	ni
```

```bash
$ ni //ni r3FW                          # split on non-words
	usr	bin	env	perl
	ni	self	license	_	
ni	https	github	com	spencertipping	ni
```

```bash
$ ni //ni r3FS                          # split on whitespace
#!/usr/bin/env	perl
$ni::self{license}	=	<<'_';
ni:	https://github.com/spencertipping/ni
```

```bash
$ ni //ni r3Fm'/\/\w+/'                 # words beginning with a slash
/usr	/bin	/env

/github	/spencertipping	/ni
```

## Vertical operator application
A situation that comes up a lot in real-world workflows is that you want to
apply some mapper code to a specific column. For example, if we want to
uppercase the third column of a dataset, we can do it like this:

```bash
$ ni //ni r3FW p'r a, b, uc(c), FR 3'
	usr	BIN	env	perl
	ni	SELF	license	_
ni	https	GITHUB	com	spencertipping	ni
```

But that requires a lot of keystrokes. More concise is to use `v` to pipe
column C to a separate ni process:

```bash
$ ni //ni r3FW vCpuc
	usr	BIN	env	perl
	ni	SELF	license	_
ni	https	GITHUB	com	spencertipping	ni
```

## Left/right juxtaposition
ni has the `+` and `^` operators to join streams vertically, but you can also
join them horizontally, row by row. This is done using `w` and `W` (for
"with"):

```bash
$ ni //ni r3FWfB
usr
ni
https
$ ni //ni r3FWfB wn100          # right-join numbers
usr	1
ni	2
https	3
$ ni //ni r3FWfB Wn100          # left-join numbers
1	usr
2	ni
3	https
```

As shown above, the output stream is only as long as the shorter input. This is
useful in conjunction with infinite generators like `n`; for example, you can
prepend line numbers to an arbitrarily long data stream like this:

```bash
$ ni //license Wn r+3
19	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
20	SOFTWARE.
21	
$ ni nE5p'a*a' Wn r+3
99998	9999600004
99999	9999800001
100000	10000000000
```

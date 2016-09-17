# Matrix operations
ni provides a handful of operations that make it easy to work with sparse and
dense matrices. The first two are `Y` (dense to sparse) and `X` (sparse to
dense), which work like this:

```bash
$ ni //ni FWr10                         # this is a dense matrix of words
	usr	bin	env	perl
	ni	self	license	_	
ni	https	github	com	spencertipping	ni
Copyright	c	2016	Spencer	Tipping	MIT	license

Permission	is	hereby	granted	free	of	charge	to	any	person	obtaining	a	copy
of	this	software	and	associated	documentation	files	the	Software	to	deal
in	the	Software	without	restriction	including	without	limitation	the	rights
to	use	copy	modify	merge	publish	distribute	sublicense	and	or	sell
copies	of	the	Software	and	to	permit	persons	to	whom	the	Software	is
```

A sparse matrix is represented as a series of `row col value` tuples:

```bash
$ ni //ni FW Yr10
0	0	
0	1	usr
0	2	bin
0	3	env
0	4	perl
1	0	
1	1	ni
1	2	self
1	3	license
1	4	_
```

`X` inverts `Y` exactly:

```bash
$ ni //ni FW Y X r10
	usr	bin	env	perl
	ni	self	license	_	
ni	https	github	com	spencertipping	ni
Copyright	c	2016	Spencer	Tipping	MIT	license

Permission	is	hereby	granted	free	of	charge	to	any	person	obtaining	a	copy
of	this	software	and	associated	documentation	files	the	Software	to	deal
in	the	Software	without	restriction	including	without	limitation	the	rights
to	use	copy	modify	merge	publish	distribute	sublicense	and	or	sell
copies	of	the	Software	and	to	permit	persons	to	whom	the	Software	is
```

## NumPy interop
You can transform dense matrices with NumPy using the `N` operator. Your code
is evaluated in an imperative context and side-effectfully transforms the input
matrix, which is called `x`.

```bash
$ ni n10p'r map a*$_, 1..10'
1	2	3	4	5	6	7	8	9	10
2	4	6	8	10	12	14	16	18	20
3	6	9	12	15	18	21	24	27	30
4	8	12	16	20	24	28	32	36	40
5	10	15	20	25	30	35	40	45	50
6	12	18	24	30	36	42	48	54	60
7	14	21	28	35	42	49	56	63	70
8	16	24	32	40	48	56	64	72	80
9	18	27	36	45	54	63	72	81	90
10	20	30	40	50	60	70	80	90	100
$ ni n10p'r map a*$_, 1..10' N'x = x + 1'
2	3	4	5	6	7	8	9	10	11
3	5	7	9	11	13	15	17	19	21
4	7	10	13	16	19	22	25	28	31
5	9	13	17	21	25	29	33	37	41
6	11	16	21	26	31	36	41	46	51
7	13	19	25	31	37	43	49	55	61
8	15	22	29	36	43	50	57	64	71
9	17	25	33	41	49	57	65	73	81
10	19	28	37	46	55	64	73	82	91
11	21	31	41	51	61	71	81	91	101
```

## Partitioned matrices
The operations above caused the entire input stream to be read into memory,
which is not very scalable. Each matrix operator allows you to specify that the
first N columns represent a partition identifier: if you indicate this, then
each matrix ends when the partition fields change.

For example, suppose we've got a bunch of words and we want to partition our
analysis by the first letter. We start by splitting that into its own column:

```bash
$ ni //license plc FWpF_ p'r/(.)(.*)/' g r10
2	016
a	
a	
a	bove
a	ction
a	ll
a	n
a	nd
a	nd
a	nd
```

Now we can apply matrix operators with the `B` qualifier, indicating that
matrices start at column B and everything left of that is the partition ID.
Let's form letter occurrence matrices by expanding into sparse form.

```bash
$ ni //license plc FWpF_ p'r split//' g r10             # split all letters
2	0	1	6
a
a
a	b	o	v	e
a	c	t	i	o	n
a	l	l
a	n
a	n	d
a	n	d
a	n	d
$ ni //license plc FWpF_ p'r split//' g YB r10          # into sparse form
2	0	0	0
2	0	1	1
2	0	2	6
a	1	0	b
a	1	1	o
a	1	2	v
a	1	3	e
a	2	0	c
a	2	1	t
a	2	2	i
$ ni //license plc FWpF_ p'r split//' gYB fABD gcfBCDA r10
2	0	6	1
a			2
a	b	v	1
a	c	i	1
a	l		1
a	n		9
a	r	s	1
a	s		1
a	s	o	1
a	u	h	1
```

At this point we have partitioned sparse matrices, and each row has the form
`first-letter row-number subsequent-letter frequency`. We can convert these to
dense matrices by using `,z` to assign a number to each subsequent letter (so
that each gets a unique column index), then sorting and using `X`.

```bash
$ ni //license plc FWpF_ p'r split//' gYBfABDgcfBCDA ,zC o XB r10
a			2
a				1
a					1
a			1
a			9
a						1
a			1				1
a								1
b			2
b			1
```

Now the matrix is in a form that NumPy can process. The `N` operator
automatically zero-fills to the right to make sure the matrix is rectangular
(as opposed to the ragged edges we have above).

I'm appending `YB,qD.01 XB` to quantize each matrix value to 0.01; this makes
the test reproducible by increasing the epsilon.

```bash
$ ni //license plc FWpF_ p'r split//' gYBfABDgcfBCDA,zCo XB \
     NB'x = linalg.svd(x)[2]' YB,qD.01 XB r10
a	0	0	1	0	0	0	0.01	0
a	0	0	0	-0.99	0	0	0	0
a	0	0	0	0	1	0	0	0
a	0	0	0	0	0	1	0	0
a	0	0	0	0	0	0	0	1
a	0	0	0	0	0	0	1	0
a	0	1	0	0	0	0	0	0
a	1	0	0	0	0	0	0	0
b	0	0	-0.99
b	0	1	0
```

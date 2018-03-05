# Matrix operations

## Sparse and Dense Matrix Operations (`X` and `Y`)
ni provides a handful of operations that make it easy to work with sparse and dense matrices. The first two are `Y` (dense to sparse) and `X` (sparse to dense), which work like this:

```bash
$ ni //ni FWr3
	usr	bin	env	perl
	ni	is_lib	caller	
	ni	self	license	_	
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
1	2	is_lib
1	3	caller
1	4	
```

`X` inverts `Y` exactly:

```bash
$ ni //ni FW fABCD Y X r4
	usr	bin	env
	ni	is_lib	caller
	ni	self	license
ni	https	github	com
```

`X` is also additive in the event of cell collisions; this makes it useful as a
reducer:

```bash
$ ni n010p'r 0, a%3, 1' X
4	3	3
```

## 1-D Matrix Operations (`Z`)
Data in row form can be flattened (lengthened?) into a column via `pF_`.

```bash
$ ni i[a b] i[c d] pF_
a
b
c
d
```

Inverting that operation, converting a column to a row with a specified number of fields is done using `Z`, which takes the number of fields as a parameter. 

```bash
$ ni i[a b] i[c d] pF_ Z2
a	b
c	d
```

In fact, `pF_` can be replaced effectively with `Z1`
 

## NumPy interop
You can transform dense matrices with NumPy using the `N` operator. Your code is evaluated in an imperative context and side-effectfully transforms the input matrix, which is called `x`.

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

`N` gets the full input stream unless you use a partition:

```bash
$ ni n4N'x = x.T'
1	2	3	4
```

This example raises an important issue: ni always imports NumPy arrays as 2D objects, never 1D (even if it's just a column of numbers). It will, however, promote any 1D arrays into column vectors.

```bash
$ ni n4N'x = reshape(x, (-1))'
1
2
3
4
```

## Partitioned matrices
The operations above caused the entire input stream to be read into memory, which is not very scalable. Each matrix operator allows you to specify that the first N columns represent a partition identifier: if you indicate this, then
each matrix ends when the partition fields change.

For example, suppose we've got a bunch of words and we want to partition our analysis by the first letter. We start by splitting that into its own column:

```bash
$ ni dev/license-for-testing plc FW Z1 p'r/(.)(.*)/' g r10
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

Now we can apply matrix operators with the `B` qualifier, indicating that matrices start at column B and everything left of that is the partition ID. Let's form letter occurrence matrices by expanding into sparse form.

```bash
$ ni dev/license-for-testing plc FWpF_ p'r split//' g r10
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
$ ni dev/license-for-testing plc FWpF_ p'r split//' g YB r10
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
$ ni dev/license-for-testing plc FWpF_ p'r split//' gYB fABD gcfBCDA r10
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
$ ni dev/license-for-testing plc FWpF_ p'r split//' \
      gYBfABDgcfBCDA ,zC o XB r10
a		2
a			1
a				1
a		1
a		9
a					1
a		1				1
a							1
b		2
b		1
```

Now the matrix is in a form that NumPy can process. The `N` operator automatically zero-fills to the right to make sure the matrix is rectangular (as opposed to the ragged edges we have above).

```bash
$ ni dev/license-for-testing plc FWpF_ p'r split//' \
     gYBfABDgcfBCDA,zCo XB \
     NB'x *= 2' YB,qD.01XB r10
a	0	4	0	0	0	0	0
a	0	0	2	0	0	0	0
a	0	0	0	2	0	0	0
a	0	2	0	0	0	0	0
a	0	18	0	0	0	0	0
a	0	0	0	0	2	0	0
a	0	2	0	0	0	2	0
a	0	0	0	0	0	0	2
b	0	4
b	0	2
```

You can use multiline code with Python and ni will fix the indentation so everything works. For example:

```bash
$ ni dev/license-for-testing plc FWpF_ p'r split//' \
     gYBfABDgcfBCDA,zCo XB \
     NB'x *= 2
        x += 1' r10
a	1	5	1	1	1	1	1
a	1	1	3	1	1	1	1
a	1	1	1	3	1	1	1
a	1	3	1	1	1	1	1
a	1	19	1	1	1	1	1
a	1	1	1	1	3	1	1
a	1	3	1	1	1	3	1
a	1	1	1	1	1	1	3
b	1	5
b	1	3
```

It also works with blocks that require indentation:

```bash
$ ni dev/license-for-testing plc FWpF_ p'r split//' \
     gYBfABDgcfBCDA,zCo XB \
     NB'if True:
          x = x + 1' r3
a	1	3	1	1	1	1	1
a	1	1	2	1	1	1	1
a	1	1	1	2	1	1	1
```

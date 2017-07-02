# Cell operations

Cell operators transform the contents of one cell at a time. To access them, you
must enter cell context using the `,` operator.

By default, cell operators only transform the first column of your data. You can
change that by passing them a column spec, for example `AB` for the first two
columns.

```bash
$ ni n5 p'r a, a*2'         # generate two columns of numbers
1	2
2	4
3	6
4	8
5	10
$ ni n5 p'r a, a*2' ,s      # sums only first column
1	2
3	4
6	6
10	8
15	10
$ ni n5 p'r a, a*2' ,sAB    # sums both columns
1	2
3	6
6	12
10	20
15	30
```

## Intification

Sometimes, you have nonnumeric data that you would like to graph. A handy
function for these cases is the intify function, `z`, which assigns a different
whole number to each unique value in a column.

```bash
$ echo -e "The\ntide\nrises\nthe\ntide\nfalls" > tide.csv
$ cat tide.csv
The
tide
rises
the
tide
falls
$ ni tide.csv ,z
0
1
2
3
1
4
```

The intify hash operator, `h`, does the same thing using hashes.

```bash
$ echo -e "The\ntide\nrises\nthe\ntide\nfalls" > tide.csv
$ cat tide.csv
The
tide
rises
the
tide
falls
$ ni tide.csv ,h
3967641545
2614616249
3746350261
865469908
2614616249
1943727641
```

A variant, `H`, also hashes, but then scales each entry to fall into the unit
interval:

```bash
$ ni tide.csv ,H
0.923788534710184
0.608762784162536
0.872265142621472
0.201507915742695
0.608762784162536
0.452559357741848
```

## Numerical transformations

ni provides four numerical transformation functions. The first two are the
natural logarithm (`l`) and the natural exponential (`e`).


```bash
$ ni n4 ,l
0
0.693147180559945
1.09861228866811
1.38629436111989
$ ni n4 ,e
2.71828182845905
7.38905609893065
20.0855369231877
54.5981500331442
```

These functions also `,e` and `,l` also take an optional paramter as a base:

```bash
$ ni n4 ,l2
0
0.999999999999998
1.58496250072115
2
$ ni n4 ,e2
2
4
7.99999999999999
16
```

The jitter function, `j`, adds a small random quantity to each cell, which is
handy for visualizing discrete quantities.

Jitter accepts two arguments: The first is the interval, which is a window
around your original value that jitter will stay within. (For example, if the
interval is 1, then the output will be within `±0.5` of the original value.)
This defaults to 1.

The second argument is the bias, which uniformly shifts every value by the
specified quantity. By default, this is 0.

```sh
$ ni n5 ,j         # jitter by ±0.5
1.38087196608922
1.61856801700512
2.91324974763314
3.79416349073558
5.28691089981028
$ ni n5 ,j2        # jitter by ±1
0.774160150120231
2.65192303824637
3.66083749763882
3.71346670826269
4.82190280992123
```

The quantize function, `q`, rounds each number to the nearest interval,
defaulting to 1.

```bash
$ ni n5 p'a*0.3'         # generate some non-integer numbers
0.3
0.6
0.9
1.2
1.5
$ ni n5 p'a*0.3' ,q      # round to the nearest integer
0
1
1
1
2
$ ni n5 p'a*0.3' ,q.5    # round to the nearest 0.5
0.5
0.5
1
1
1.5
$ ni n6 ,q2              # round to the nearest multiple of 2
2
2
4
4
6
6
```

If a number is equidistant from either end of the interval, ni will round it
upwards.

```bash
$ ni n05
0
1
2
3
4
$ ni n05 ,q4
0
0
4
4
4
```

## Streaming numeric transformations

The three streaming numeric operators, sum (`s`), delta (`d`), and average
(`a`), give a running statistic of the column contents.

```bash
$ ni n5 ,s    # running sum
1
3
6
10
15
$ ni n5 ,d    # running difference
1
1
1
1
1
$ ni n5 ,a    # running average
1
1.5
2
2.5
3
```

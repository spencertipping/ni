# Cell operations

Use `,` to enter cell context.

## Intification

Sometimes, you have nonnumeric data that you would like to graph. A handy
function for these cases is the intify function, `z`, which assigns an integer
to each unique value in a column.

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

The intify hash operator, `h`, does the same thing, except instead of integers,
it assigns each value a unique hash.

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
2866659357
2144378426
1536230278
3162218338
2144378426
1918171572
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

The jitter function, `j`, adds a small random quantity to each cell. It takes
two arguments: The first is a range. For example, if the range is given as 1,
then all numbers will be between `x - 0.5` and `x + 0.5`. TODO: Bias.

log
exp
jitter
quantize

## Streaming numeric transformations

sum
delta
average

## Search and replace

:D

# Cell operations


## Intification

Sometimes, you have nonnumeric data that you would like to graph. A handy
function for these cases is the intify function, A common case is needing to turn nonnumerical ni
offers a couple of functions that turn each unique value into its own integer.

```bash
$ echo -e "The\ntide\nrises\nthe\ntide\nfalls" > data.csv
$ cat data.csv
The
tide
rises
the
tide
falls
$ ni data.csv ,z
0
1
2
3
1
4
```
intify compact
intify hash

## Numerical transformations

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

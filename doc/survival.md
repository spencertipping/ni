## Survival commands
![!](http://spencertipping.com/ni2.png)

ni is terse and shares many properties with write-only languages. Because of
this you'll probably want to make use of its documentation/previewing
operators, which always go at the end:

```sh
$ ni -n1s10x/ --explain
        --cat fd:0
-n      --number
-1s     --address 1 --sum
-10x/   --address 10 --canard /

$ ni perl:5,6 -n1s10x/ --trace
0               5
1       -n      1       5
2       -1s     1       5
3       -10x/   5
0               6
1       -n      2       6
2       -1s     2       11
3       -10x/   5.5
```

You can also ask ni for a list of supported operators, quasi-file syntaxes, and
configuration variables:

```sh
$ ni --help
$ ni --help-operators
$ ni --help-quasifiles
$ ni --help-env
```

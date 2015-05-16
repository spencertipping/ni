## Survival commands
![!](http://spencertipping.com/ni2.png)

ni is terse and shares many properties with write-only languages. Because of
this you'll probably want to make use of its documentation/previewing
operators, which always go at the end:

```sh
$ ni -n1R/+ -10m// --explain
        --cat fd:0
-n      --number
-1R/+   --address 1 --fold / +
-10m//  --address 10 --map / /

$ { echo '5'; echo '6'; } | ni -n1R/+ -10m// --trace
0               5
1       -n      1       5
2       -1R/+   1       5
3       -10m//  5
0               6
1       -n      2       6
2       -1R/+   2       11
3       -10m//  5.5
```

You can also ask ni for a list of supported operators, quasi-file syntaxes, and
configuration variables:

```sh
$ ni --help
$ ni --help-builtins
$ ni --help-operators
$ ni --help-quasifiles
$ ni --help-env
```


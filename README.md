![ni!](http://spencertipping.com/ni.png)

# Examples
Running total of numbers:

```sh
$ seq 1000 | ni -s              # sum column 0 (default operand)
$ seq 1000 | ni -0s             # sum column 0 (explicit operand)
$ seq 1000 | ni -0^+r           # reduce + over column 0 (^ is quotation)
```

Average is similar:

```sh
$ seq 1000 | ni -a              # running average 0
$ seq 1000 | ni -0a
$ seq 1000 | ni -n1^+rx1/       # prepend #, sum 1, swap, divide each
```

## JSON
Get the value of a JSON field:

```sh
$ echo '{"foo":{"bar":[1,2,3]}}' > json
$ ni json -j .foo.bar[1]                # compound reference
$ ni json -j .foo -j .bar -j [1]        # split reference
```

Extract an array of JSON objects:

```sh
$ echo '[{"foo":1},{"bar":2}]' > json
$ ni json -j []                         # expand array into columns
```

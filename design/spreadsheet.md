# Spreadsheet interface
`a` to `q` ambivalently refer to cells or columns, depending on how you use
them. You can use conditional expressions to construct lazy index ranges:

```sh
$ ni m'r b[a == a0]'            # collect column B for equal ranges of A
```

Operators shade the interpretation of types:

```sh
$ ni m'r a + b'                 # coerce to numbers
$ ni m'r "#{a}#{b}"'            # strings
$ ni m'r a <=> b'               # compare as strings
$ ni m'r ai <=> bi'             # coerce to integers, then naturally compare
$ ni m'r a[5]'                  # sixth row in column A
$ ni m'r a[b]'                  # error: no implicit conversion
$ ni m'r a[bi]'                 # b'th row in column A
$ ni m'r a == 5'                # emit indexes while a[i] == 5
```

Cells compare to regular expressions by matching and implement successors, so
you can do this:

```sh
$ ni m'r b[a../foo/]'           # take values from column B until A =~ /foo/
```

You can access the whole spreadsheet using `_`, which has the following
properties:

```sh
$ ni m'r _[0]'                  # return first line as a string
$ ni m'r _[0, 0]'               # same as 'r a0'
$ ni m'r a[_../foo/]'           # column A until the line matches /foo/
$ ni m'r _[_../foo/]'           # lines until the one containing /foo/
```

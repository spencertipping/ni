# Function definitions
ni lets you define aliases using the `defexpander` internal function. These
support arguments of arbitrary parse types and provide basic substitution. For
example, let's define a shorthand for fractional numbers:

```bash
$ mkdir fractional
$ echo fractional.pl > fractional/lib
$ cat >fractional/fractional.pl <<'EOF'
defexpander ['/frac', n => pc integer, step => pc number],
            'n$n', 'pa * $step';
EOF
```

Now we can load the library and use the new operator:

```bash
$ ni --lib fractional frac 10 .5
0.5
1
1.5
2
2.5
3
3.5
4
4.5
5
$ ni --lib fractional frac4.5
0.5
1
1.5
2
```

You can also define a nullary function, which is just a regular shorthand:

```bash
$ ni --run 'defexpander "/evens", qw[np2*a]' evens r10
2
4
6
8
10
12
14
16
18
20
```

## Using Perl functions
You can use an arbitrary Perl expression instead of an expansion template:

```bash
$ mkdir fractional2
$ echo fractional2.pl > fractional2/lib
$ cat >fractional2/fractional2.pl <<'EOF'
defexpander ['/frac', n => pc integer, step => pc number],
  sub {
    my %args = @_;
    ("+n$args{n}", "pa * $args{step}");
  };
EOF
$ ni --lib fractional2 frac 10 .5
0.5
1
1.5
2
2.5
3
3.5
4
4.5
5
```

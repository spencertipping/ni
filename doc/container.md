# Containerized pipelines
Some ni operators depend on tools you may not want to install on your machine.
If you want to use those operators anyway, though, you can run them inside a
Docker container with the tools installed. ni provides the `C` operator to
containerize a stream transformation:

```sh
$ ni //ni Cubuntu[gc] r10
```

The above is executed roughly like this, except that ni pipes itself into Perl
rather than invoking itself by name:

```sh
$ ni //ni \
  | docker run --rm -i ubuntu ni gc \
  | ni r10
```

A common use case is for something like NumPy:

```sh
$ ni n100 Cadreeve/numpy[N'x = x + 1']
```

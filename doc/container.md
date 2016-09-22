# Containerized pipelines
```lazytest
# These tests only get run in environments where docker is installed
# (centos 5 uses i386 libraries and doesn't support docker, for example).
if ! [[ $SKIP_DOCKER ]]; then
```

Some ni operators depend on tools you may not want to install on your machine.
If you want to use those operators anyway, though, you can run them inside a
Docker container with the tools installed. ni provides the `C` operator to
containerize a stream transformation:

```bash
$ ni nE4 Cubuntu[gr4] O
1000
100
10
1
```

The above is executed roughly like this, except that ni pipes itself into Perl
rather than invoking itself by name:

```sh
$ ni nE4 \
  | docker run --rm -i ubuntu ni gr4 \
  | ni O
```

A common use case is for something like NumPy:

```bash
$ docker build -q -t ni-test/numpy - <<EOF > /dev/null
FROM ubuntu
RUN apt-get update
RUN apt-get install -y python-numpy
CMD /bin/bash
EOF
$ ni n100 Cni-test/numpy[N'x = x + 1'] r4
2
3
4
5
```

```lazytest
fi                      # $HAVE_DOCKER (lazytest condition)
```

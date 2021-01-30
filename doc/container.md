# Containerized pipelines
```lazytest
# These tests only get run in environments where docker is installed
# (centos 5 uses i386 libraries and doesn't support docker, for example).
if ! [[ -e /nodocker ]]; then
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
RUN apt-get install -y python3-numpy
CMD /bin/bash
EOF
$ ni n100 Cni-test/numpy[N'x = x + 1'] r4
2
3
4
5
```

## Dynamic images
The examples above are a little awkward in that (1) they require you to tag the
docker images, and (2) they require a separate command to build them in the
first place. To get around this, ni lets you define image generators and
includes predefined ones for Ubuntu and Alpine:

```bash
$ ni n100 CU+python3-numpy+sbcl[N'x = x + 1' l'(1+ a)'] r4
3
4
5
6
$ ni n100 CA+python3+py3-numpy+sbcl@testing[N'x = x + 1' l'(1+ a)'] r4
3
4
5
6
```

## Running in an existing container
ni can run `docker exec` and do the same interop it does when it creates a new
container.

```bash
$ docker run --detach -i --name ni-test-container ubuntu >/dev/null
$ ni Eni-test-container[n100g =\>/tmp/in-container Bn] r4
1
10
100
11
$ [[ -e /tmp/in-container ]] || echo 'file not in host (good)'
file not in host (good)
$ ni Eni-test-container[/tmp/in-container] | wc -l
100
$ docker rm -f ni-test-container >/dev/null
```

```lazytest
fi                      # -e /nodocker (lazytest condition)
```

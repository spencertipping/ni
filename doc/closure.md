# Data closures
Sometimes it's useful to bundle data with ni so that it's available on a
different filesystem. Data closures do exactly this.

There are two kinds of data closures, memory-resident and disk-backed, with the
obvious implications. Both types of data closures are accessible to scripts you
write inside ni; for example:

```bash
$ ni n5                         # some data
1
2
3
4
5
$ ni ::foo[n5]                  # ...in a memory-resident closure
```

Once you have a closure, it's visible to mappers, row filters, and any other
embedded script as a constant. It also becomes accessible as its own data
stream:

```
$ ni ::foo[n5] //:foo                   # closures are streams
1
2
3
4
5
$ ni ::foo[n5] n1p'r split /\n/, foo'   # and strings inside languages
1	2	3	4	5
```

The operative feature of closures is that they travel with ni, for instance
into a docker container:

```lazytest
if ! [[ -e /nodocker ]]; then
```

```bash
$ ni ::foo[n5] Cubuntu[//:foo]
1
2
3
4
5
$ ni ::foo[n5] Cubuntu[n1p'r split /\n/, foo']
1	2	3	4	5
```

```lazytest
fi                      # -e /nodocker
```

Disk-backed closures have almost exactly the same semantics, and are
automatically deleted when ni exits:

```bash
$ rm -r /tmp/* || :
$ ni :@foo[n10] //@foo e[wc -l]         # disk-backed data closure
10
$ ls /tmp | wc -l
0
$ ni :@foo[nE5] :@bar[nE4] //@foo //@bar gr9
1
1
10
10
100
100
1000
1000
10000
```

Disk-backed closures turn into file URIs inside language environments.

```bash
$ ni :@foo[nE6] \
      n1p'open my $fh, "<", foo =~ /^file:\/\/(.*)/ or die $!;
          print while <$fh>;()' e[wc -c]
6888896
```

They also travel with ni into tempfiles on remote systems, and ni maps the
names accordingly:

```lazytest
if ! [[ -e /nodocker ]]; then
```

```bash
$ ni :@foo[nE5] :@bar[nE4] Cubuntu[//@foo //@bar gr9]
1
1
10
10
100
100
1000
1000
10000
$ ni :@foo[nE6] Cubuntu[ \
    n1p'open my $fh, "<", foo =~ /^file:\/\/(.*)/ or die $!;
        print while <$fh>;()'] e[wc -c]
6888896
```

```lazytest
fi                      # -e /nodocker
```

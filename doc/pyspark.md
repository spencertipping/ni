# PySpark interop
```lazytest
# All of these tests require Docker, so skip if we don't have it
if ! [[ -e /nodocker ]]; then
```

ni's `P` operator compiles a series of stream operators into PySpark. It takes
a context as its first argument, then a lambda of stream operations. Like other
ni commands, data is streamed into and out of the PySpark context. For example,
using the `L` (for "local") Spark profile:

```bash
$ ni Cgettyimages/spark[PL[n10] \<o]
1
2
3
4
5
6
7
8
9
10
```

```lazytest
fi              # -e /nodocker
```

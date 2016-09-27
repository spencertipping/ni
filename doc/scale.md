# Pipeline scaling
The scaling operator `S` lets you work around bottlenecks by adding horizontal
scale to a section of your pipeline. For example, suppose we have this:

```bash
$ ni nE6 p'sin(a/100)' rp'a >= 0' =\>slow-sine-table e[wc -l]
500141
```

On a multicore machine, this will run more slowly than it should because
`p` processes data serially. We can parallelize it across four processes like
this:

```bash
$ ni nE6 S4[p'sin(a/100)' rp'a >= 0'] =\>parallel-sine-table e[wc -l]
500141
$ diff <(ni slow-sine-table o) <(ni parallel-sine-table o) | head -n10
```

`S` works by reading blocks of input and finding line boundaries. It then sends
groups of lines to the child processes as their input fds become available.

Scaling makes no guarantees about the ordering of the output rows, nor where
exactly the input will be split. In practice the input splits tend to be large
for performance reasons.

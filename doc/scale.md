# Pipeline scaling
The scaling operator `S` lets you work around bottlenecks by adding horizontal
scale to a section of your pipeline. For example, suppose we have this:

```bash
$ ni nE6 p'sin(a/100)' > slow-sine-table
```

On a multicore machine, this will run more slowly than it should because
`p` processes data serially. We can parallelize it across multiple processes
like this:

```bash
$ ni nE6 S4[p'sin(a/100)'] > parallel-sine-table
$ diff <(ni slow-sine-table o) <(ni parallel-sine-table o)
```

`S` works by reading blocks of input and finding line boundaries. It then sends
groups of lines to the child processes as their input fds become available.

# Visualizing data
There are two ways to use ni to visualize things. One is to take a data stream
and plot it with the `v` (visualize) operator, which is parameterized by the
backend you want to use:

```sh
$ ni nE4p'sin(a / 1000)' vgd            # visualize with gnuplot with dots
```

The other way is to launch a web UI that lets you iterate quickly.

```sh
$ ni --js
http://localhost:8090
```

**TODO: more stuff**

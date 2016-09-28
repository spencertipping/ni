# Monitors
If you run a pipeline that takes longer than a couple of seconds, ni will emit
monitor output to standard error. It looks like this:

```sh
$ ni nE9S8p'sin(rand())' rp'a > 0.1' ,q0.01 czn
 6106K  2085K/s  -8 905930
 4223K  1427K/s  24 0.735911990151528
 3672K  1251K/s  16 0.3174551373783
  985K   339K/s -19 0.45
 1329K   456K/s -32 ["count"]
```

Each row corresponds to the output side of one pipeline operation:

```sh
$ ni --explain nE9S8p'sin(rand())' rp'a > 0.1' ,q0.01 czn
["n",1,1000000001]
["row_fixed_scale",8,[["perl_mapper","sin(rand())"]]]
["perl_grepper","a > 0.1"]
[["quantize",[1,0],0.01]]
["count"]
["sink_null"]
```

(`sink_null` produces no output, so its monitor will never appear.)

There are four columns:

```
     +------------------------- total output data (from this operator)
     |        +---------------- output data rate
     |        |   +------------ output data "pressure" (more below)
     |        |   |      +----- data preview, or --explain for the operator
     |        |   |      |        (the display alternates between the two)
 6106K  2085K/s  -8 905930
```

## Throughput and data pressure
Data pipeline performance isn't entirely straightforward, particularly when
buffering operators are involved. Most ni operators, however, are written to
stream data quickly from input to output -- in which case the slowest operator
will govern the overall pipeline speed. For example, we can write an
artificially slow map function to reduce the throughput everywhere:

```sh
$ ni nE7 pa+1 zn                        # a few seconds
$ ni nE7 p'sleep 0.1; a + 1' zn         # quite a lot longer
```

Throughput and output data won't help you identify the pipeline bottleneck in
these situations because the pipeline is effectively moving in lock-step. This
is where the concept of data pressure comes in.

If you run the second example above, you'll see monitor output like this:

```
 2815K    90K/s 115 ["n",1,10000001]
 2752K    88K/s-116 ["perl_mapper","sleep 0.1; a + 1"]
```

The output pressure from `nE7` is 115, and the output pressure of the Perl
command is -116. Intuitively this makes sense: the Perl command is creating
backpressure against `n`, and `sink_null` is, relatively to the Perl command,
creating the opposite (i.e. it appears to have no backpressure at all, acting
like a vacuum).

ni calculates the pressure by measuring the time spent waiting on IO requests.
The pressure is `10 * log2(output_time / input_time)`, so in the example above
`n` was producing data about 3000 times as fast as the Perl process was
consuming it (and the Perl process was producing data about 3000 times _slower_
than `sink_null` was willing to consume it).

## Dealing with bottlenecks
The easiest way to solve a bottleneck is to [horizontally scale](scale.md) the
slow sections of the pipeline. This isn't always possible, for example if you
need to sort things, but when it is possible it should produce significant
speedups. For example:

```sh
$ ni nE7 S8p'sleep 0.1; a + 1' zn
33014K   645K/s  49 4356592
21735K   433K/s-117 ["row_fixed_scale",8,[["perl_mapper","sleep 0.1; a + 1"]]]
```

Now the Perl function appears to be only ~32x slower than `n` -- unexpectedly
fast for two reasons. First, `S` maintains large memory buffers that it fills
quickly at pipeline startup; that will distort initial readings, though it will
ultimately even out. Second, `n` produces rows whose length increases as we
read more input; so the `sleep 0.1` in the Perl command has less of an impact
the further we get into the stream. (This is an example of data-dependent
throughput, which happens a lot in practice.)

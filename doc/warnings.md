# Things to look out for
![img](http://spencertipping.com/ni.png)

![img](http://spencertipping.com/ni2.png)

## `r` and pipe signals
Internally, commands like `r10` will read ten rows and then break the input
pipe, causing the writing process to receive a `SIGPIPE` and terminate.
Normally this is what you want to happen, particularly when the process would
otherwise generate data forever. But there are some situations where it causes
problems; for example:

```bash
$ ni n1000000 =\>not-a-million-things r5
1
2
3
4
5
$ echo $(( $(cat not-a-million-things 2>/dev/null | wc -l) < 1000000 ))
1
```

(The output file might not even get created, depending on when the pipe signal
arrives.)

Here's what I got the last time I tried it:

```sh
$ wc -l not-a-million-things
3100 not-a-million-things
```

This happens because the ni process implementing `=` does basically this:

```
while data = read stdin:
  write data to stdout          # connected to r5
  write data to child process   # connected to \>not-a-million-things
```

Once `r5` (which is implemented with `head -n5`) receives enough data, it
exits, closing its input stream. This causes the next write to generate a pipe
signal and kill the process.

### Workaround
`r` is designed to break pipes, so you need to prepend an operator that will
either buffer or consume the stream first. Any sorting operator will do this
automatically:

```bash
$ ni n1000000 =\>a-million-things gr5
1
10
100
1000
10000
$ wc -l < a-million-things
1000000
```

If you weren't planning to sort your data, though, a better alternative is to
use `B`, the buffering operator, with a null buffer:

```sh
# UPDATE: this no longer works reliably; it depends on which signal is used to
# kill the pipeline. This will be fixed when I merge r/oo into develop.
$ ni n1000000 =\>a-million-things Bn r5
1
2
3
4
5
$ wc -l < a-million-things
1000000
```

The null buffer has no storage overhead; it just forwards data as it arrives
and ignores broken pipes.

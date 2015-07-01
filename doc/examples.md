# Examples
## Map/reduce word count
```sh
$ ni data.txt -FW1k1ga ^1st+1                           # local
$ ni hdfs:data.txt -H ^FW1k1 ^a^1st+1                   # hadoop
```

- `-FW`: shorthand for `-F '\W+'`: split on non-words
- `-1k1`: address `-k1` to field 1, which juxtaposes each word with 1
- `-ga`: group/aggregate
- `-1s`: within each aggregated group, sum column 1's values
- `-t+1`: take last row

## Index JSON dataset by geohash
```sh
$ ni data -r'r j0.name, ge(j0.latitude, j0.longitude, 8)' -10fg
```

`j0` means "`%0` interpreted as JSON". It notationally bypasses the caching
otherwise necessary to support multiple-access. `ge` is a builtin function to
encode geohashes. It's possible that the actual geohash and JSON handling will
be done outside the Ruby context to reduce the number of dependencies or
improve performance; if this happens, the difference won't be observable unless
you use some type of debugging/profiling tool.

## Real-time queueing
ni provides some operators that implement disk-backed queues for bandwidth
consistency. The main purpose of queueing like this is to augment the
OS-provided pipe buffer to improve overall performance, though ni queues are
also designed to maintain data movement, throttling only when absolutely
necessary (i.e. low disk space on the host).

```sh
$ ni http://data-source/data.txt -qd 16^gc
```

The assumption here is that we want to download `data.txt` as fast as possible,
then have 16 sort/count processes each pulling records as quickly as they can.

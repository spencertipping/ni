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

Non-MR word count (using gnu sort):

```sh
$ ni data.txt -FWvgc
```

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

## Bloom filters
```sh
$ ni data.txt -FWvJb [ /usr/share/dict/words -i b:@ ]
```

- `-FW`: split on non-words
- `-v`: flatmap columns to vertical form
- `-Jb`: join against a bloom filter (TODO: nope nope nope)
- `[ ... ]`: quote ni invocation and use as a data source
    - `-i`: redirect stream into a quasifile, return quasifile name
    - `b:@`: create a temporary quasifile that is a bloom filter

This can be much faster than sorting both sides of the data and doing a linear
join:

```sh
$ ni data.txt -FWvj /usr/share/dict/words
```

Bloom filters can be tuned using configuration variables `bf.h` or `bf.p`,
which specify true-positive confidence in bits or probability, respectively:

```sh
$ ni data.txt -FWvJb [ bf.h=8 /usr/share/dict/words -i b:@ ]
```

# Examples
## Map/reduce word count
```sh
$ ni data.txt -FW1k1gp'r f0, sum a0i1'                  # local
$ ni hdfs:data.txt -hFW1k1/p'r f0, sum a0->i1']         # local
$ ni hdfs:data.txt -HFW1k1/p'r f0, sum a0->i1']         # hadoop
```

- `-FW`: shorthand for `-F '\W+'`: split on non-words
- `-1k1`: address `-k1` to field 1, which juxtaposes each word with 1
- `-g`: group
- `-p`: execute perl code
    - `r x, y`: emit a row of values
    - `f0`: value of first field
    - `a0i1`: integer interpretation of field 1 forward-aggregated by field 0

Using the `-A/--aggregate` operator:

```sh
$ ni data.txt -FW1k1gA^1st+1
$ ni hdfs:data.txt -HFW1k1/A^1st+1
```

Non-MR word count (using command-line sort):

```sh
$ ni data.txt -FWvgc                                    # prepend count
$ ni data.txt -FWvgcx                                   # append count
```

## Index JSON dataset by geohash
```sh
$ ni data -m'r j0.name, ge(j0.latitude, j0.longitude, 8)' -xg
```

`j0` means "`f0` interpreted as JSON". It notationally bypasses the caching
otherwise necessary to support multiple-access. `ge` is a builtin function to
encode geohashes.

## Real-time queueing
ni provides some operators that implement compressed, disk-backed queues for
bandwidth consistency. The main purpose of queueing like this is to augment the
OS-provided pipe buffer to improve overall performance, though ni queues are
also designed to maintain data movement, throttling only when absolutely
necessary (i.e. low disk space on the host).

```sh
# TODO: fix this; having -d be its own operator is egregiously verbose -- not
# to mention the considerable ergonomic overhead immediately following a Q.
$ ni http://data-source/data.txt -Qd16[gc]
```

The assumption here is that we want to download `data.txt` as fast as possible,
then have 16 sort/count processes each pulling records as quickly as they can.

## Record extraction
Variants of `-t` allow you to extract lines matching a specific pattern,
internally using `grep -E`:

```sh
$ ni hdfs:/data/source -Ht/'foo.*bar'
```

## Bloom filters
```sh
$ ni data.txt -FWvJb [ /usr/share/dict/words -i b:@ ]
```

- `-FW`: split on non-words
- `-v`: flatmap columns to vertical form
- `-Jb`: join against a bloom filter (**TODO:** nope nope nope)
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

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

## Encode/decode geohashes
```sh
$ ni data.tsv -x 'ge 12'
$ ni encoded -x gd
```

## Encode/decode JSON
```sh
$ ni data.json -0x 'row jd [.foo.bar .bif.baz:1]'
$ ni n:100 -00x 'row je [.foo.bar .bif.baz:1] 2l'
```

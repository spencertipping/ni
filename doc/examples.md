# Examples
## Map/reduce word count
```sh
$ ni data.txt -F \\W+ -1k1ga ^1st+1                             # local
$ ni hdfs:data.txt -H [ -F \\W+ -1k1 ] [ -a ^1st+1 ]            # hadoop
```

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

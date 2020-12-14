# Multithreaded `sort` experiment
Bilow and I have both been thinking that it would be good to have a multi-stream
`sort` variant that used `sort -m` to merge already-sorted sub-outputs. This
would change the way you write map/reduce pipelines:

```sh
# current strategy: sort everything in one big operation
$ ni <data> Sn[<map>] g <reduce>

# proposed strategy: assume map outputs are sorted and use sort -m
$ ni <data> SM<n>[<map> g] <reduce>
```

I want to find out how much faster it is to sort individual batches like this.
Here's the experiment I'm running on the [20201201 non-multistream export of
Wikipedia](https://dumps.wikimedia.org/enwiki/20201201/enwiki-20201201-pages-articles.xml.bz2);
I believe you can run the same commands and get identical SHAs.

I'm using `ni` at commit c88469d230b7aff62ac7fe07afc2fccc20c090ef.


## The setup
```sh
# deterministic split into 16 equally-sized lz4 files:
$ ni /mnt/data/enwiki-20201201.xz Wn%16 S\>z4
$ ls -lh {0..15}
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 0
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 1
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 10
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 11
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 12
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 13
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 14
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 15
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 2
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 3
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 4
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 5
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 6
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 7
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 8
-rw-r--r-- 1 spencertipping spencertipping 2.7G Dec 14 12:31 9
```

First let's verify the input read rate:

```sh
$ lz4 -dc -m {0..15} | pv > /dev/null
6.12GiB 0:00:09 [ 684MiB/s] [        <=>                                       ]
```

That should saturate `sort`'s input so I think we're in good shape.


## Server 1: 16 CPUs, 80GB memory
Hardware is 16x Xeon E5620 with 80GB memory and 12x 2TB HDD arranged in a
20TB software RAID-6 array with an ext4 filesystem and ~5.5TB of free space.
Sequential IO runs at about 700-800MB/s on this machine (as measured by
`pv 160GB-file > /dev/null`).

OK, so since we're working with 16 CPUs there are two ways we can sort things:
we can use one big sort with one big buffer (let's go with 72GB), or we can
split 16 ways and give each one 4.5GB, then use `sort -m` to combine those on
the fly. I'll `sha512sum` all of the outputs to verify that we're getting the
same data out.

```sh
# one big sort
$ time lz4 -dc -m {0..15} | sort --buffer-size=72g --parallel=16 | sha512sum
8c23f1e8287d2946df504361c1981bdaf3799545deb16d3a0a3d46862d97c66e2e6f8956f596178d161f6be9894f41734bb1b9a1d7b5134e473c543120e00549  -

real    81m56.429s
user    340m4.444s
sys     25m6.580s

# multiple small sorts
$ time sort -m <(lz4 -dc 0 | sort --buffer-size=4500m) \
               <(lz4 -dc 1 | sort --buffer-size=4500m) \
               <(lz4 -dc 2 | sort --buffer-size=4500m) \
               <(lz4 -dc 3 | sort --buffer-size=4500m) \
               <(lz4 -dc 4 | sort --buffer-size=4500m) \
               <(lz4 -dc 5 | sort --buffer-size=4500m) \
               <(lz4 -dc 6 | sort --buffer-size=4500m) \
               <(lz4 -dc 7 | sort --buffer-size=4500m) \
               <(lz4 -dc 8 | sort --buffer-size=4500m) \
               <(lz4 -dc 9 | sort --buffer-size=4500m) \
               <(lz4 -dc 10 | sort --buffer-size=4500m) \
               <(lz4 -dc 11 | sort --buffer-size=4500m) \
               <(lz4 -dc 12 | sort --buffer-size=4500m) \
               <(lz4 -dc 13 | sort --buffer-size=4500m) \
               <(lz4 -dc 14 | sort --buffer-size=4500m) \
               <(lz4 -dc 15 | sort --buffer-size=4500m) | sha512sum
```

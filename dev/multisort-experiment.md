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
Here's the experiment I'm running on the [20201201 **non-multistream** export of
Wikipedia](https://dumps.wikimedia.org/enwiki/20201201/enwiki-20201201-pages-articles.xml.bz2);
I believe you can run the same commands and get identical SHAs.

I'm using `ni` at commit bba55536875d8753f86be6f974934c13576a3185.


## The setup
```sh
# deterministic split into 16 equally-sized lz4 files:
$ ni /mnt/data/enwiki-20201201.xz f^,Z16 S\>z4
$ ls -lh {0..15}

```

First let's make sure we can read data quickly:

```sh
$ lz4 -dc -m {0..15} | pv > /dev/null
```

OK, so since we're working with 16 CPUs there are two ways we can sort things:
we can use one big sort with one big buffer (let's go with 72GB), or we can
split 16 ways and give each one 4.5GB, then use `sort -m` to combine those on
the fly. I'll `sha512sum` all of the outputs to verify that we're getting the
same data out.

```sh
$ 
```


## Server 1: 16 CPUs, 80GB memory
Hardware is 16x Xeon E5620 with 80GB memory and 12x 2TB HDD arranged in a
20TB software RAID-6 array with an ext4 filesystem and ~5.5TB of free space.
Sequential IO runs at about 700-800MB/s on this machine (as measured by
`pv 160GB-file > /dev/null`).

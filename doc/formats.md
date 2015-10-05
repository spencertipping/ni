# ni data formats
ni handles data formats in a somewhat complicated way, but I haven't yet come
up with any better strategy. The design constraints are these:

1. Must be able to consume TSV from files, subprocesses, and stdin.
2. Should be able to represent numbers more efficiently than in ASCII when
   communicating with cooperating subprocesses.
3. Must be able to detect common compressed/archive formats by content (i.e.
   without looking at the file extension).
4. IPC protocol should allow for easy record-skipping and other fast
   operations, while being simple enough that it's easy to implement the driver
   for it.

Given these constraints, here's how it works:

```
   file1.gz fd  file2.xz fd
             |  |
      gzip -dc  xz -dc
             |  |
      tsv->bin  tsv->bin                  bin->tsv
             |  |                         |
$ ni  file1.gz  file2.xz  -m 'length f0'  [| less]
                           |
                           binary->perl->binary
                           (perl library does this)
```

Specifically, the pipeline itself is all binary. Files and output are TSV by
default, but input files are type-detected. It's also ok to store results in
the binary format: because endianness is encoded and it has a magic number, ni
will be able to use that data later on.

**TODO:** The above model won't work for stuff like remotely-addressed
  resources; i.e. suppose something puts data onto HDFS: we don't want to rope
  the user into downloading it just to run another hadoop job, for example.

**TODO:** We really should have a protocol that defaults to indirect references
  but provides support for heredoc-style embedding (which we can do with a
  sufficiently high-entropy marker, though scanning for it might be slow ...
  maybe chunking instead?).

## File type detection
Detecting file types is complicated by a few factors:

1. Each ni process uses nonblocking IO to read file data; this allows ni to
   minimize the amount of IPC going on.
2. File type detection logic technically needs 265 bytes to be able to detect
   all possibilities (notably tar files), but some files are shorter.
3. Not all data sources are files; some are quasifiles like `n:10` or `ni:self`
   that address memory or dynamic contents. This means not all sources are
   backed by a file descriptor.
4. Because of the indirection created by file type detection, each subprocess
   requires two fds instead of one, plus one for the original file, increasing
   the likelihood that we'll run out of file descriptors. We have a hard limit
   of 1024 for `select()`, which is the only POSIX-specified polling mechanism.

We have a few possible failure modes:

1. We'll run out of fds, which forces ni to fork. The graph above uses six fds,
   so it will probably take a while to hit a critical fd limit. But ni
   ultimately needs to be able handle things like this.
2. If we use blocking IO, we could fork too much and run out of memory. I have
   yet to see this happen on Linux, even with ~2k processes.

I think I'd rather assume one fork per pipeline stage. It should be cheap to
pass binary data between processes; most of nfu's speed issues are probably due
to its TSV/fields conversion.

## Binary format details
### Magic numbers
We want something human-recognizable and not mistakable for text. I like the
byte-string "ni!\000" for this: the null byte makes it an improbable header for
almost everything else.

### Changing endianness mid-stream
More of an open question than an active issue; it's unclear to me how often
this is likely to happen. The only way to actually get mixing is if we're
reading records that have been serialized by someone else but we haven't yet
had an opportunity to fix their endianness. This would happen if you
concatenated binary files from different architectures, for example.

So it seems like in practice, byte order changes are very infrequent. Certainly
no need to do anything per-record.

### Seeking
The assumption is that data is being streamed, so seeking is more about saving
CPU cycles than being able to skip around in the data. That said, is there
still a reason to have some navigational context (e.g. a local skip list)?

The purpose of skipping over a record would be because you already know it's
irrelevant. That happens in a few cases:

1. Sampling randomly. If you use a Poisson process, you know how many records
   to skip -- which is faster than uniformly predicating each one. Speed is
   also critical here (presumably; otherwise you wouldn't subsample), so fast
   multi-record seeking is worthwhile.
2. Linear-joining against sparse, ordered data: you can load up a bunch of
   records into a large buffer, then binary-search that buffer if you can seek
   through it. (Marginally faster than a linear scan; fewer comparisons,
   anyway.) Could significantly improve comparison count when mergesorting data
   with sorted runs.
3. Backward seeking, for any reason? I can't think of a decent reason to do
   this.

It's easy to write the offsets if we commit to buffering more than one record
before writing them (which we should probably do anyway). OK, so let's do this
and have log-N byte offset pointers per record: one ahead, two ahead, four
ahead, ..., 256 ahead. Any can be zero to indicate an unknown.

### Record encoding
A record looks like this:

```
struct record {                 // all offsets relative to beginning of record
  uint8_t  magic[2];            // always "nr"; helps debugging
  uint16_t nfields;
  uint64_t pointers[8];         // next, 2, 4, 8, ..., 256th next byte offsets
  uint32_t fields[nfields];     // high 2 bits for type, low 30 bits for offset
  byte     record_data[...];    // a blob
};
```

There are three cell types: int64, double, and byte array, encoded like this:

```
enum field_type {
  INT        = 0,
  DOUBLE     = 1,
  BYTE_ARRAY = 2,
  RESERVED   = 3
};
```

Ints and doubles are encoded natively; byte arrays are packed with their length
first, like this:

```
struct byte_array {
  uint32_t length;
  byte     data[...];
};
```

**TODO:** Factor record shape into a non-repeated reference field.

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
                           binary->perl
```

Specifically, the pipeline itself is all binary. Files and output are TSV by
default, but input files are type-detected. It's also ok to store results in
the binary format: because endianness is encoded and it has a magic number, ni
will be able to use that data later on.

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

## Binary format details
The binary format is designed to minimize CPU load, particularly when dealing
with numbers. Conceptually, here's what is encoded:

```
record       ::= magic-number uint32 length8
                              uint32 nfields
                              field-spec *
                              field-data *

sub-record   ::= sub-number   uint32 length8
                              uint32 nfields
                              field-spec *
                              field-data *

magic-number ::= 'NI' 0x0021           # 0x0021 in native-endian encoding
sub-number   ::= 'ni' 0x0021           # 0x0021 in native-endian encoding
field-spec   ::= uint32 (type[1:3] offset8[4:32])
field-data   ::= padding* { double | int64 | byte-array | sub-record }
byte-array   ::= uint32 length  byte * \0 *

type : {0   -> native-endian int64,
        1   -> double,
        2   -> byte array,
        3   -> sub-record
        4-7 -> reserved}
```

Like other data types, the binary format is detected by its magic number. The
0x21 byte in the magic number may change in the future to indicate a future
revision of the format, though it will always be greater than 0x20. The null
byte is included to prevent any confusion with text data, and the magic number
is repeated on each record to simplify inspection/debugging.

Fields are always 8-byte aligned relative to the record offset, and records are
guaranteed to be a multiple of 8 bytes long. The `length8` field in the record
header includes the record's header length and all field data, so if they're
laid out in memory, then `r1 == (void*) r0 + 8 * r0.length8`.

Although the schema technically supports records up to 32GB in size,
implementations are only required to support sizes up to 64MB.

### Example record
```
# { int64 f0 = 5, int64 f1 = -1, double f2 = 1.0, byte-array f3 = 'foo' }
"NI" 21 00              # little-endian encoding        offset >| 4
  08 00 00 00           # length8                       offset >| 8
  04 00 00 00           # nfields                       offset >| 12
  b00100000 00 00 00    # int64 f0 @offset 32           offset >| 16
  b00101000 00 00 00    # int64 f1 @offset 40           offset >| 20
  b00110001 00 00 00    # double f2 @offset 48          offset >| 24
  b00111010 00 00 00    # byte-array f3 @offset 56      offset >| 28
  xx xx xx xx           # padding to 8 bytes            offset >| 32
  05000000 00000000     # int64 f0 value                offset >| 40
  ffffffff ffffffff     # int64 f1 value                offset >| 48
  00000000 0000f03f     # double f2 value               offset >| 56
  03000000 "foo"xx      # byte-array f3 value           offset >| 64
# next record starts at offset 64
```

# Binary decoding
ni's row transform operators won't work on binary data because they seek to the
nearest newline. If you want to parse binary data you should use the `b`
operator, which does block reads and provides a byte cursor.

## Generating binary data
You can't use `r()` to emit binary unless you want newlines, but you can print
bytes directly to standard output using `wp` (write-packed) or `ws` (write
string). For example, let's synthesize a one-second wav file:

```bash
$ ni n1p'wp "A4VA8VvvVVvvA4V",
         qw|RIFF 176436 WAVEfmt 16 1 2 44100 176400 4 16 data 176400|' \
     +n44100p'my $v = 32767 * sin a*440*tau/44100;
              wp "ss", $v, $v' \
  > test.wav
```

Note that normally you'd specify the endian-ness of `s` by saying `s<` instead.
However, old versions of Perl don't support endian modifiers so I have to leave
it out of the examples here.

## Perl decoder
The decoding interface consists of four functions:

- `$offset = bi`: absolute offset of the next byte
- `$bytes = pb(n)`: peek and return N bytes
- `$bytes = rb(n)`: consume and return N bytes
- `@value = rp("packstring")`: read, unpack, and consume bytes by pack pattern

For example, we can decode samples from the WAV file above. `bp` means "binary
read with perl":

```bash
$ ni test.wav bp'rp "A4VA8VvvVVvvA4V" if bi == 0;       # skip the header
                 r rp"ss"' r10
2052	2052
4097	4097
6126	6126
8130	8130
10103	10103
12036	12036
13921	13921
15752	15752
17521	17521
19222	19222
```

A faster approach is to use the `bf` operator to read fixed-length packed
records (internally it uses more efficient logic to manage the queue of
incoming bytes):

```bash
$ ni test.wav bf'ss' r-15r10
10103	10103
12036	12036
13921	13921
15752	15752
17521	17521
19222	19222
20846	20846
22389	22389
23844	23844
25205	25205
```

If we wanted to find the dominant frequencies, for example (the `,qB.01`
quantizes the magnitudes so the test case doesn't depend on float rounding):

```bash
$ ni test.wav bp'bi?r rp "ss":rb 44' fA N'x = fft.fft(x, axis=0).real' \
     Wn rp'a <= 22050' OB r5,qB.01
441	45263289.95
14941	755.22
7341	745.63
8461	667.75
12181	620.78
```

### Packed searching/lookup
Perl's normal data structures are optimized for performance rather than small
memory footprint, but sometimes you're up against a hard memory limit. For cases
like this, you can use things like [bloom filters](bloom.md) or, for associative
lookups, binary-search tables.

Let's suppose we want to build a lookup table for the `sin` function. In text
we'd do this:

```sh
$ ni numbers... p'r a, sin(a)' > lookup-table
```

Then we'd have TSV/ascii, which isn't very efficient. If we `pack` each value
into `Nd` (network byte-order `long` followed by a `double`, so 12 bytes per
entry), then we get a much smaller table:

```bash
$ ni nE4 op'wp"Nd", a, sin(a)' > binary-lookup
```

Importantly, we use `o` before `p` because the entries need to be sorted by
lookup key.

#### Lookups
Let's use this lookup table in a random-access way. We can read the whole table
into memory using `ri`, "read into", and then we can use the `bsflookup`
function to binary-search fixed records. Its signature is:

```pl
bsflookup($packed_table,        # contents of binary-lookup
          $key_unpacker,        # "N"
          $record_length,       # 12
          $target_key,          # a number
          $value_unpacker)      # "x4d" (remember to skip over the key)
```

```bash
$ ni nE4 eshuf p'^{ri $table, "<binary-lookup"}
                 r a, sin(a), bsflookup $table, "N", 12, a, "x4d"' \
               rp'b ne c' e'wc -l'      # any records have a failed lookup?
0
```

If a lookup fails, `bsflookup` will return `undef`. You can access the insertion
location for the missing record using `bsf`, which takes the first four args to
`bsflookup` and returns a record index. (`bsflookup` uses `bsf` internally.)

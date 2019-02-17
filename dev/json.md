# JSON library internals/design
Source in [core/json/json.pl.sdoc](../core/json/json.pl).

The library has two purposes. One is to act as a tolerably-fast compatibility
fix for Perl environments that have no JSON support. It provides two functions,
`json_encode` and `json_decode`, that handle full JSON objects. Decoding runs
at about 5MiB/s on my machine, which isn't great but is much faster than
`JSON::PP` from CPAN.

The other purpose is to provide an insanely fast value extractor for data
mining workflows. This is ideal for trying experiments on rich JSON objects
where you only need a couple of fields.

## Fast value extraction
**TODO**

# JSON operators
## Background
Perl has a standard JSON library since 5.14, but it's written in pure Perl and
decodes at about 1MiB/s (vs the ~60MiB/s for the native version, but that isn't
installed by default). Older versions of Perl don't include any JSON support at
all.

To work around this, ni implements a medium-speed pure Perl JSON parser
(~5MiB/s) and some very fast alternatives for cases where you don't need to do
a full object parse.

Internally, ni uses its own JSON library for pipeline serialization and to
generate the output of `--explain`.

## Full encode/decode
### Perl driver
```bash
$ ni //license FWp'json_encode [F_]' r4
["ni","https","github","com","spencertipping","ni"]
["Copyright","c",2016,"Spencer","Tipping","MIT","license"]
[]
["Permission","is","hereby","granted","free","of","charge","to","any","person","obtaining","a","copy"]
```

`json_decode` inverts and always returns a reference:

```bash
$ ni //license FWp'json_encode [F_]' p'r @{json_decode a}' r4
ni	https	github	com	spencertipping	ni
Copyright	c	2016	Spencer	Tipping	MIT	license

Permission	is	hereby	granted	free	of	charge	to	any	person	obtaining	a	copy
```

### Ruby driver
**TODO**: this is necessary because older Rubies don't provide any JSON
support.

## Sparse extraction
It's uncommon to need every field in a JSON object, particularly when you're
testing specific hypotheses on rich data. This makes it likely that JSON
decoding will be pipeline bottleneck simply due to the ratio of bytes
pre-vs-post-decode. ni helps to mitigate this by providing a very fast
destructuring operator that works like `jq` (but 2-3x faster):

```bash
$ ni //license FWpF_ p'r pl 3' \
     p'json_encode {type    => 'trigram',
                    context => {w1 => a, w2 => b},
                    word    => c}' \>jsons
jsons
$ ni jsons r5
{"context":{"w1":"https","w2":"github"},"type":"trigram","word":"com"}
{"context":{"w1":"github","w2":"com"},"type":"trigram","word":"ni"}
{"context":{"w1":"com","w2":"ni"},"type":"trigram","word":"c"}
{"context":{"w1":"ni","w2":"c"},"type":"trigram","word":"Spencer"}
{"context":{"w1":"c","w2":"Spencer"},"type":"trigram","word":"MIT"}
```

A destructuring specification consists of a comma-delimited list of extractors:

```bash
$ ni jsons D:w1,:w2,:word r5
https	github	com
github	com	ni
com	ni	c
ni	c	Spencer
c	Spencer	MIT
```

### Types of extractors
The example above used the `:field` extractor, which means "find the first
occurrence of a field called `field`, and return its value."

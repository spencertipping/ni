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
$ ni i[hi there] i[my friend] p'json_encode [F_]'
["hi","there"]
["my","friend"]
```

`json_decode` inverts and always returns a reference:

```bash
$ ni i[hi there] i[my friend] p'json_encode [F_]' p'r @{json_decode a}'
hi	there
my	friend
```

## Sparse extraction
It's uncommon to need every field in a JSON object, particularly when you're testing specific hypotheses on rich data. This makes it likely that JSON decoding will be pipeline bottleneck simply due to the ratio of bytes pre-vs-post-decode. ni helps to mitigate this by providing a very fast destructuring operator that works like `jq` (but 2-3x faster):

```bash
$ ni i[who let the dogs out?! who? who?? who???] Z1 p'r pl 3' r5 \
     p'json_encode {type    => 'trigram',
                    context => {w1 => a, w2 => b},
                    word    => c}'
{"context":{"w1":"let","w2":"the"},"type":"trigram","word":"dogs"}
{"context":{"w1":"the","w2":"dogs"},"type":"trigram","word":"out?!"}
{"context":{"w1":"dogs","w2":"out?!"},"type":"trigram","word":"who?"}
{"context":{"w1":"out?!","w2":"who?"},"type":"trigram","word":"who??"}
{"context":{"w1":"who?","w2":"who??"},"type":"trigram","word":"who???"}
```

A destructuring specification consists of a comma-delimited list of extractors:

```bash
$ ni i[who let the dogs out?! who? who?? who???] Z1 p'r pl 3' r5 \
     p'json_encode {type    => 'trigram',
                    context => {w1 => a, w2 => b},
                    word    => c}' \
      D:w1,:w2,:word
let	the	dogs
the	dogs	out?!
dogs	out?!	who?
out?!	who?	who??
who?	who??	who???
```

### Types of extractors
The example above used the `:field` extractor, which means "find the first occurrence of a field called `field`, and return its value."

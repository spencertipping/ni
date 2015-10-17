# Operator reference
Short operators have the following conventions:

- Frequency should be inversely proportional to typing effort on a QWERTY
  keyboard.
- In typical usage, uppercase operators rarely follow operators with optional
  arguments; optional arguments tend to begin with uppercase letters.
- Any operator whose argument is both mandatory and always multi-character can
  be joined with its argument, e.g. `-p'foo'` for Perl code.
- Unlike nfu, each operator is designed to handle a wide variety of use cases
  with optional arguments. This enables better space optimization.

## High-level changes from nfu
- ni can compile code in arbitrary languages
- ni saves state by rewriting itself, allowing you to easily customize it
- ni automatically creates shorthands to minimize #characters/operation
- ni is much more concise and has more powerful lambda notation
- Field addressing happens before the operator: `-10f` instead of `-f10`
- Partition is now aggregation: `-A^gc` instead of `--partition %0 ^gc`
- Each language supports key-reduction in its API
- Command-line arguments are concatenative: `f1 -g f2` != `f1 f2 -g`
- Quasifiles are read/write

## General-purpose stream operators
The following prefixes are reserved for column addressing:

- `.`: range operator
- `,`: juxtaposition operator; enables field indexes > 9
- `0-9`: fields

Short   | Long          | Operands      | Description
--------|---------------|---------------|------------
`-a`    | average       | window-spec   | running/windowed average
`-A`    | aggregate     | lambda        | aggregate by addressed fields
`-b`    |               |               |
`-B`    |               |               |
`-c`    | count         |               | `uniq -c` for addressed columns
`-C`    | clojure       | code          | pipe through clojure
`-d`    | distribute    | lambda-list   | distribute across subprocesses
`-D`    | Distribute    | lambda-list   | distribute across machines
`-e`    | encode        | format-spec   | encodes stream into a format
`-E`    |               |               |
`-f`    | fields        |               | reorder, drop, create fields
`-F`    | fieldsplit    | split-spec    | split each column
`-g`    | group         |               | group rows by addressed column(s)
`-G`    |               |               |
`-h`    | ladoop        | m r           | local simulation of `-H`
`-H`    | hadoop        | m r           | hadoop streaming, emits qfile out
`-i`    |               |               |
`-I`    |               |               |
`-j`    | join          | join-spec     | join data by addressed columns
`-J`    |               |               |
`-k`    | constant      | value         | emits a constant value
`-K`    | kill          |               | eats data; emits nothing
`-l`    | log           | log-spec      | log or other numerical compression
`-L`    | exp           | log-spec      | invert a `--log` operation
`-m`    | ruby          | code          | pipe through ruby
`-M`    | octave        | code          | pipe through octave
`-n`    | number        |               | prepend line number or intify
`-N`    |               |               |
`-o`    | order         |               | order rows by addressed column(s)
`-O`    | Order         |               | reverse-order rows
`-p`    | perl          | code          | pipe through perl
`-P`    | python        | code          | pipe through python
`-q`    | quant         | quant-spec    | quantize
`-Q`    | queue         | queue-spec    | queue against disk
`-r`    | read          |               | dereferences quasifiles from stream
`-R`    | write         | quasifile     | writes stream to quasifile
`-s`    | sum           |               | running sum
`-S`    | delta         |               | delta (inverts --sum)
`-t`    | take          | line-spec     | take selected lines
`-T`    | tcp           | port lambda   | runs a TCP server
`-u`    | uniq          |               | `uniq` for addressed columns
`-U`    |               |               |
`-v`    | vertical      | [fieldlist]   | chops line into multiple lines
`-V`    | horizontal    |               | join lines where addr field is blank
`-w`    |               |               |
`-W`    | web           | port lambda   | runs a very simple webserver
`-x`    | xchg          |               | exchanges first and addressed//second
`-X`    | switch        |               | rotates addressed columns to front
`-y`    |               |               |
`-Y`    |               |               |
`-z`    | zip           | qfile         | zip columns from specified qfile
`-Z`    | scala         | code          | pipe through scala
`-+`    |               |               |
`-=`    | save          | qfile         | fork stream and save to qfile
`-$`    | shell         | command       | pipe through shell command
`-?`    |               |               |

## Lambda forms
Each lambda form is compiled into, and replaced with, the name of a separate
program. For example, writing `ni ... [ foo ]` is the same as writing `ni ...
X`, where `X` is the name of a shell script that executes `ni foo`.

### Quoted commands
```sh
$ ni ... -$ 'grep foo'
$ ni ... -$ grep[ foo ]         # same thing, but better quoting support
$ ni ... -$ ni[ foo ]           # a way to quote ni, but more likely...
$ ni ... -$ [ foo ]             # ... you'd say this instead
```

### Bracket operators and syntax
**Pending redesign**

- `[ ... ]`: list as quasifile
- `@[ ... ]`: forking list as quasifile
- `-[ ... ]`: list as action: append results
- `-@[ ... ]`: forking list as action: interleave in arbitrary order

- `{ ... }`: decisional list as quasifile
- `@{ ... }`: forking decisional list as quasifile
- `-{ ... }`: decisional list as action: replace results
- `-@{ ... }`: forking decisional list as action: interleave in arbitrary order

Additional notation includes:

- `^x` = `[ -x ]`
- `^^x` = `[ [ -x ] ]`
- `^x^y` = `[ -x [ -y ] ]`
- `@^x^@^y` = `@[ -x [ @[ -y ] ] ]`
- `4[ x y ]` = `[ x y x y x y x y ]`
- `@3[ x y ]` = `@[ x y x y x y ]`
- `-3[ x y ]` = `-[ x y x y x y ]`
- `-@3[ x y ]` = `-@[ x y x y x y ]`
- `2^x` = `[ [ -x ] [ -x ] ]` (useful with `-d`)

The `^` operator is structural, which means that arguments are parsed normally.
As a result, `^r 'foo'` turns into `[ -r 'foo' ]` because `-r` requires an
argument.

Square-bracket lists support the following identities, up to record order as
indicated above. `X` and `Y` stand for arbitrary lists of options.

Shell form             | List form
-----------------------|--------------
`ni X | ni Y`          | `ni X Y`
`ni X; ni Y`           | `ni X -[ Y ]`
`ni Y; ni X`           | `ni X -/ [ Y ]`
`ni X | tee f; ni f Y` | `ni X -@[ Y ]`
`ni X sh:'ni Y'`       | `ni X [ Y ]` or `ni X ^Y`

### Decisional lists
Decisional lists allow you to predicate on a record's first field. All matching
is done verbatim, so you'll need to transform your data before branching.
Decisional lists are written like this:

```sh
$ ni ... { val1 -gA^r 'foo' -n , \
           val2 -gc ... , \
           ... } ...
```

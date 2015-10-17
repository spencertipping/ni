# Operator reference
Short operators have the following conventions:

- Frequency should be inversely proportional to typing effort on a QWERTY
  keyboard.
- Any operator whose argument is both mandatory and always multi-character can
  be joined with its argument, e.g. `-p'foo'` for Perl code.
- Unlike nfu, each operator is designed to handle a wide variety of use cases
  with optional arguments. This enables better space optimization.
- Lambda arguments are typically coerced, e.g. `-As` to mean `-A ^s`.

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

Reserved non-operators:

Short   | Description
--------|------------
`-E`    | exponential indicator: 1E5 = 100000
`-=`    | fork stream and save to qfile
`-@`    | fork stream and save to variable

The following are default bindings and can be changed by modifying `home/conf`:

Short   | Long          | Operands      | Description
--------|---------------|---------------|------------
`-a`    | average       | window-spec   | running/windowed average
`-A`    | aggregate     | lambda        | aggregate by addressed fields
`-b`    |               |               |
`-B`    |               |               |
`-c`    | count         |               | `uniq -c` for addressed columns
`-C`    | clojure       | code          | pipe through clojure
`-d`    | distribute    | dist-spec ... | prefix for distributed computation
`-D`    |               |               |
`-e`    | encode        | format-spec   | encodes stream into a format
`-f`    | fields        |               | reorder, drop, create fields
`-F`    | fieldsplit    | split-spec    | split each column
`-g`    | group         |               | group rows by addressed column(s)
`-G`    | groupuniq     |               | `-gu`
`-h`    | ladoop        | m r           | local simulation of `-H`
`-H`    | hadoop        | m r           | hadoop streaming, emits qfile out
`-i`    |               |               |
`-I`    |               |               |
`-j`    | join          | join-spec     | join data by addressed columns
`-J`    | json          | access-spec   | extract specified paths within json
`-k`    | constant      | value         | emits a constant value
`-K`    | kill          |               | eats data; emits nothing
`-l`    | log           | lambda        | emits data as logging output
`-L`    | l2/exp        | log-spec      | numerical compression/expansion
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
`-X`    |               |               |
`-y`    |               |               |
`-Y`    |               |               |
`-z`    | zip           | qfile         | zip columns from specified qfile
`-Z`    |               |               |
`-$`    | shell         | command       | pipe through shell command

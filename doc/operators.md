# Stream operators
Short   | Long          | Operands      | Description
--------|---------------|---------------|------------
        | address       | fieldlist     | set address of next command
        | c             | [flags] code  | pipe through C99
--------|---------------|---------------|------------
`-a`    | aggregate     | transform     | aggregate rows by first field
`-A`    |               |               |
`-b`    | buffer        | size          | preload data, buffering into memory
`-B`    | diskbuffer    | size          | preload data, buffering to disk
`-c`    | count         |               | `uniq -c` for addressed columns
`-C`    | clojure       | [flags] code  | pipe through clojure
`-d`    |               |               |
`-D`    | duplicate     | qfile         | duplicate into quasifile
`-e`    |               |               |
`-E`    |               |               |
`-f`    | fields        | [N]           | reorder, drop, create fields
`-F`    | fieldsplit    | regexp        | split into columns on regexp
`-g`    | group         |               | group rows by addressed column(s)
`-G`    |               |               |
`-h`    |               |               |
`-H`    | hadoop        | m r           | hadoop streaming, emits qfile out
`-i`    | into          | [quasifile]   | writes into qfile, emits qfile name
`-I`    | from          |               | reads from qfiles
`-j`    | join          | [flags] qfile | join data by addressed columns
`-J`    | java          | [flags] code  | pipe through java
`-k`    |               |               |
`-K`    |               |               |
`-l`    |               |               |
`-L`    |               |               |
`-m`    |               |               |
`-M`    | octave        | [flags] code  | pipe through octave
`-n`    | number        |               | prepend line number (init 1)
`-N`    |               |               |
`-o`    | order         |               | order rows by addressed column(s)
`-O`    | rorder        |               | reverse-order rows
`-p`    | perl          | [flags] code  | pipe through perl
`-P`    | python        | [flags] code  | pipe through python
`-q`    |               |               |
`-Q`    |               |               |
`-r`    | ruby          | [flags] code  | pipe through ruby
`-R`    | R             | [flags] code  | pipe through R
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
`-x`    | canard        | [flags] code  | pipes through canard
`-X`    | shell         | [flags] code  | pipes through shell command
`-y`    |               |               |
`-Y`    |               |               |
`-z`    | zip           | qfile         | zip columns from specified qfile
`-Z`    | scala         | [flags] code  | pipe through scala
`-+`    |               |               |
`-/`    |               |               |
`-=`    |               |               |
`-!`    |               |               |
`-:`    |               |               |
`-.`    |               |               |
`-,`    |               |               |
`-?`    |               |               | prefix: set operators
`-#`    |               |               | prefix: numerical operators
`-%`    |               |               | prefix: exact statistical operators
`-^`    | prepend       | qfile         | prepends qfile to stream
`-[`    | begin         |               | pushes new empty stream onto stack
`-]`    | end           |               | pops stream, appending to parent
`-{`    |               |               |
`-}`    |               |               |

## Shorthands
- `^...` = `[ -... ]`

## Set operators
Short   | Long          | Operands      | Description
--------|---------------|---------------|------------
`-?d`   | set-diff      | [flags] qfile | set difference by addressed field
`-?i`   | intersection  | [flags] qfile | set intersection
`-?s`   | subset        | [flags] qfile | running subset predicate
`-?S`   | superset      | [flags] qfile | running superset predicate
`-?u`   | union         | [flags] qfile | set union

## Numerical operators
Short   | Long          | Operands      | Description
--------|---------------|---------------|------------
`-#l`   | log           | [base=2]      | log of each number
`-#L`   | exp           | [base=2]      | exponentiate each number
`-#p`   | root          | [exp=2]       | root of each number
`-#P`   | pow           | [exp=2]       | power of each number
`-#q`   | quant         | [q=1]         | quantize each number

## Exact statistical operators
Short   | Long          | Operands      | Description
--------|---------------|---------------|------------
`-%a`   | average       | [flags]       | running average
`-%h`   | entropy       | [flags]       | running entropy
`-%n`   | ntiles        | N             | exact n-tiles of stream
`-%s`   | stddev        | [flags]       | running standard deviation
`-%v`   | variance      | [flags]       | running variance

# Stream operators
Short   | Long          | Operands      | Description
--------|---------------|---------------|------------
        | address       | fieldlist     | set address of next command
`-a`    | aggregate     | transform     | aggregate rows by first field
`-A`    |               |               |
`-b`    | buffer        | size          | preload data, buffering into memory
`-B`    | diskbuffer    | size          | preload data, buffering to disk
`-c`    | count         |               | `uniq -c` for addressed columns
`-C`    |               |               |
`-d`    | distribute    | [flags] code  | distribute lines among workers
`-D`    |               |               |
`-e`    |               |               |
`-E`    |               |               |
`-f`    | fields        | [N]           | reorder, drop, create fields
`-F`    | fieldsplit    | regexp        | split into columns on regexp
`-g`    | group         |               | group rows by addressed column(s)
`-G`    |               |               |
`-h`    |               |               |
`-H`    | hadoop        | m [r]         | hadoop streaming, emits qfile out
`-i`    | into          | [quasifile]   | writes into qfile, emits qfile name
`-I`    | from          |               | reads from qfiles
`-j`    | join          | [flags] qfile | join data by addressed columns
`-J`    |               |               |
`-k`    | keep          | code          | keep rows for which code is true
`-K`    |               |               |
`-l`    |               |               |
`-L`    |               |               |
`-m`    | map           | code          | runs code per line
`-M`    |               |               |
`-n`    | number        |               | prepend line number (init 1)
`-N`    |               |               |
`-o`    | order         |               | order rows by addressed column(s)
`-O`    | rorder        |               | reverse-order rows
`-p`    |               |               |
`-P`    |               |               |
`-q`    |               |               |
`-Q`    |               |               |
`-r`    | reduce        | code          | n-ary reduce entire stream
`-R`    | fold          | code          | binary reduce entire stream
`-s`    | sum           |               | running sum
`-S`    | delta         |               | delta (inverts --sum)
`-t`    | take          | line-spec     | take selected lines
`-T`    | tcp           | port lambda   | runs a TCP server
`-u`    | uniq          |               | `uniq` for addressed columns
`-U`    |               |               |
`-v`    | vertical      | [fieldlist]   | chops line into multiple lines
`-V`    |               |               |
`-w`    |               |               |
`-W`    | web           | port lambda   | runs a very simple webserver
`-x`    |               |               |
`-X`    |               |               |
`-y`    |               |               |
`-Y`    |               |               |
`-z`    | zip           | qfile         | zip columns from specified qfile
`-Z`    |               |               |
`-+`    |               |               |
`-/`    |               |               |
`-=`    |               |               |
`-!`    |               |               |
`-:`    |               |               |
`-?`    |               |               |
`-.`    |               |               |
`-,`    |               |               |
`-#`    |               |               | prefix: numerical operators
`-%`    |               |               | prefix: exact statistical operators
`-^`    | prepend       | qfile         | prepends qfile to stream
`-[`    | begin         |               | pushes new empty stream onto stack
`-]`    | end           |               | pops stream, appending to parent
`-{`    |               |               |
`-}`    |               |               |

## Shorthands
- `^...` = `[ -... ]`

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

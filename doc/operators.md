# Operator reference
## General-purpose stream operators
Short   | Long          | Operands      | Description
--------|---------------|---------------|------------
        | c             | [flags] code  | pipe through C99
        |               |               |
`-a`    | aggregate     | transform     | aggregate rows by first field
`-A`    |               |               |
`-b`    |               |               |
`-B`    |               |               |
`-c`    | count         |               | `uniq -c` for addressed columns
`-C`    | clojure       | code          | map through clojure
`-d`    | distribute    | lambda-list   | distribute across subprocesses
`-D`    | duplicate     | qfile         | duplicate into quasifile
`-e`    | encode        | codec         | interpret with codec
`-E`    | decode        | codec         | generate with codec
`-f`    | fields        | [N]           | reorder, drop, create fields
`-F`    | fieldsplit    | regexp        | split into columns on regexp
`-g`    | group         |               | group rows by addressed column(s)
`-G`    | grep          | pattern       | pipe through egrep
`-h`    |               |               |
`-H`    | hadoop        | m r           | hadoop streaming, emits qfile out
`-i`    | into          | [quasifile]   | writes into qfile, emits qfile name
`-I`    | from          |               | reads from qfiles
`-j`    | join          | [flags] qfile | join data by addressed columns
`-J`    | jvm           |               | JVM language prefix
`-k`    | constant      | value         | emits a constant value
`-K`    |               |               |
`-l`    |               |               |
`-L`    |               |               |
`-m`    |               |               |
`-M`    | octave        | [flags] code  | pipe through octave
`-n`    | number        |               | prepend line number or intify
`-N`    |               |               |
`-o`    | order         |               | order rows by addressed column(s)
`-O`    | rorder        |               | reverse-order rows
`-p`    | perl          | [flags] code  | pipe through perl
`-P`    | python        | [flags] code  | pipe through python
`-q`    | queue         | [profile]     | queue against disk
`-Q`    | sql           |               | SQL prefix
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
`-x`    |               |               |
`-X`    |               |               |
`-y`    |               |               |
`-Y`    |               |               |
`-z`    | zip           | qfile         | zip columns from specified qfile
`-Z`    |               |               |
`-+`    |               |               |
`-/`    | subst         | { vars }      | substitute in prior terms
`-=`    |               |               |
`-!`    | shell         | command       | pipe stream through shell command
`-:`    | conf[ig]      | var=value     | set configuration variable
`-.`    | fork          | lambda        | fork through lambda/decisional
`-,`    |               |               |
`-@`    | address       | fieldlist     | set address of next command
`-?`    |               |               | prefix: set operators
`-#`    |               |               | prefix: numerical operators
`-%`    | interleave    | qfile         | breadth-first concatenation
`-^`    | prepend       | qfile         | prepends qfile to stream
`-[`    | begin         |               | pushes new empty stream onto stack
`-]`    | end           |               | pops stream, appending to parent
`-{`    |               |               | stream through decisional
`-}`    |               |               | n/a

### Join flags
With no flags, ni sorts both sides and joins on the first column. Joined values
(not the right-hand join column itself) are zipped and the join is left-outer.

Flag    | Description
--------|------------
`^`     | left-hand data is already sorted
`:`     | right-hand data is already sorted
`%`     | outer left/right join
`=`     | inner join

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
`-#a`   | average       | [flags]       | running average
`-#h`   | entropy       | [flags]       | running entropy
`-#l`   | log           | [base=2]      | log of each number
`-#L`   | exp           | [base=2]      | exponentiate each number
`-#n`   | ntiles        | N             | exact n-tiles of stream
`-#p`   | root          | [exp=2]       | root of each number
`-#P`   | pow           | [exp=2]       | power of each number
`-#q`   | quant         | [q=1]         | quantize each number
`-#s`   | stddev        | [flags]       | running standard deviation
`-#v`   | variance      | [flags]       | running variance

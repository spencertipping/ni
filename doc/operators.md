# Operator reference
## General-purpose stream operators
Short   | Long          | Operands      | Description
--------|---------------|---------------|------------
        | c             | [flags] code  | pipe through C99
        |               |               |
`-a`    | aggregate     | transform     | aggregate rows by first field
`-A`    |               |               |
`-b`    | buffer        | size          | preload data, buffering into memory
`-B`    | diskbuffer    | size          | preload data, buffering to disk
`-c`    | count         |               | `uniq -c` for addressed columns
`-C`    | clojure       | [flags] code  | pipe through clojure
`-d`    |               |               |
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
`-J`    | java          | [flags] code  | pipe through java
`-k`    | constant      | value         | emits a constant value
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
`-q`    | sql           | db query      | sqlite3 query with transient table
`-Q`    | psql          | db query      | postgres query with transient table
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
`-/`    | subst         | { vars }      | substitute in prior terms
`-=`    |               |               |
`-!`    |               |               |
`-:`    | conf[ig]      | var=value     | set configuration variable
`-.`    |               |               |
`-,`    |               |               |
`-@`    | address       | fieldlist     | set address of next command
`-?`    |               |               | prefix: set operators
`-#`    |               |               | prefix: numerical operators
`-%`    |               |               |
`-^`    | prepend       | qfile         | prepends qfile to stream
`-[`    | begin         |               | pushes new empty stream onto stack
`-]`    | end           |               | pops stream, appending to parent
`-{`    |               |               | begins canard block
`-}`    |               |               | ends canard block

### Code flags
With no flags, the code is invoked once per line. It uses `row()` to emit
results, fields are available using `%0`, `%1`, ..., `%N`, and number of fields
is `%#`.

Flag    | Description
--------|------------
`+`     | distribute work across multiple local processes
`*`     | distribute work across multiple remote processes
`%`     | all lines in one reduction; `%i` become generators of column values
`?`     | code is used as a line filter; falsy returns delete the line
`/`     | omit field splitting; `%0` contains the whole line and `%#` is 1
`@`     | split fields into array; `String f[]` is available
`:`     | name fields using first line (i.e. assume column headers)
`!`     | all lines in one reduction; `%i` become **concrete arrays** of values

**TODO:** `%` prefix won't work because single-column loading will either force
or lose the others.

Note that `/` prevents column-wise binary coding from being used, which may
result in slower execution. `!` may use an arbitrary amount of space or cause
OOME's.

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

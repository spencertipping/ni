# Operator reference
## General-purpose stream operators
Short   | Long          | Operands      | Description
--------|---------------|---------------|------------
        | c             | [flags] code  | pipe through C99
        |               |               |
`-a`    | aggregate     | lambda        | aggregate rows by first field
`-A`    | amb           | lambda-list   | choose fastest alternative
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
`-J`    |               |               | JVM language prefix
`-k`    | constant      | value         | emits a constant value
`-K`    |               |               |
`-l`    |               |               |
`-L`    |               |               |
`-m`    |               |               |
`-M`    |               |               |
`-n`    | number        |               | prepend line number or intify
`-N`    |               |               | numeric language prefix
`-o`    | order         |               | order rows by addressed column(s)
`-O`    | rorder        |               | reverse-order rows
`-p`    | perl          | code          | map through perl
`-P`    | Perl          | code          | reduce through perl
`-q`    | queue         | [profile]     | queue against disk
`-Q`    | sql           |               | SQL prefix
`-r`    | ruby          | code          | map through ruby
`-R`    | Ruby          | code          | reduce through ruby
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
`-y`    | python        | code          | map through python
`-Y`    | Python        | code          | reduce through python
`-z`    | zip           | qfile         | zip columns from specified qfile
`-Z`    |               |               |
`-+`    |               |               |
`-/`    | subst         | { vars }      | substitute in prior terms
`-=`    |               |               |
`-!`    | shell         | command       | pipe stream through shell command
`-:`    | conf[ig]      | var=value     | set configuration variable
`-.`    |               |               |
`-,`    |               |               |
`-?`    |               |               | prefix: set operators
`-#`    |               |               | prefix: numerical operators
`-%`    | interleave    | qfile         | breadth-first concatenation
`-^`    | prepend       | qfile         | prepends qfile to stream

## Bracket operators and syntax
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
- `2^x` = `[ -x -x ]` (unlikely to be what you want)
- `2^^x` = `[ [ -x ] [ -x ] ]` (useful with `-d` and `-A`)

Square-bracket lists support the following identities, up to record order as
indicated above. `X` and `Y` stand for arbitrary lists of options.

Shell form             | List form
-----------------------|--------------
`ni X | ni Y`          | `ni X Y`
`ni X; ni Y`           | `ni X -[ Y ]`
`ni Y; ni X`           | `ni X -^ [ Y ]`
`ni X | tee f; ni f Y` | `ni X -@[ Y ]`
`ni X sh:'ni Y'`       | `ni X [ Y ]` or `ni X ^Y`

## Decisional lists
Decisional lists allow you to predicate on a record's first field. All matching
is done verbatim, so you'll need to transform your data before branching.

## Language interfacing
Languages have two modes, map and reduce, specified by lowercase and uppercase
letters, respectively. Map mode invokes your code on each record individually;
reduce mode invokes your code once and provides a lazy stream of records. Some
languages are prefixed:

- `-J[cC]` | `--[Cc]lojure`: Clojure (requires `lein`)
- `-J[jJ]` | `--[Jj]ava`: Java (requires `mvn`)
- `-J[sS]` | `--[Ss]cala`: Scala (requires `sbt` -- TODO)
- `-M[oO]` | `--[Oo]ctave`: Octave
- `-M[rR]` | `--[Rr]`: R

## Join flags
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

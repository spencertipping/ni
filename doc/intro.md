# Introduction

## What is ni?
ni is an awesome data science tool, etc. MORE.

## Grammar
Like bash, ni is a pipeline constructor: It strings processes together by
connecting one's stdin to another's stdout. ni's pipe symbols are implied: `ni g
c` is the same as `ni g | ni c`.

For example, here's word count implemented first in bash, then in ni (with the
corresponding options vertically aligned):

```
$ cat file |perl -lne 'print for split /\W+/' |sort |uniq -c
$ ni  file FW pF_                             g     c
```

ni typically doesn't require whitespace between commands:

```
$ ni n3 g       # generate three integers and then sort
1
2
3
$ ni n3g        # no need for whitespace
1
2
3
```

The only exceptions are cases when the parse would be ambiguous without it,
which tends to happen when operators take options whose names overlap with other
operators or the shell erases characters. (MORE?) Typically, ni will complain if
it can't parse something.

## Escaping terminal characters
Since ni operates on the bash command line, the user must have some awareness of
how bash transforms the argument string thingy. MORE. ni sees only post-quoted
arguments.

Quoting examples (<, >, and '): MORE.

```
$ ni n3e'sort -r'
3
2
1
$ ni n3e[ sort -r ]             # easy way to quote arguments
3
2
1
```

Erasure examples: MORE.

```
```

## Debugging
ni offers an `--explain` option that will display the parse tree, etc. MORE.

```sh
$ ni --explain FW pF_ g c
["split_regex","(?^:[^\\w\\n]+)"]
["perl_mapper","F_"]
["row_sort","-t","\t"]
["count"]
```

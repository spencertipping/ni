# Canard
Canard is a right-to-left concatenative programming language inspired by
[Joy](http://www.kevinalbrecht.com/code/joy-mirror/joy.html), but with some of
[Forth's](https://en.wikipedia.org/wiki/FORTH) low-level minimalism thrown in.
It also contains some conveniences like maps, which are totally not
minimalistic but are really useful. All values are immutable.

The original (non-working) implementation of Canard in x86-64 machine language
is
[here](https://github.com/spencertipping/canard/blob/circular/bin/canard.md).
This version changes some semantics like list-handedness and value
representation, but generally carries the spirit of the original language.

## Syntactic elements
Canard is homoiconic and is parsed like Lisp:

- `[a b c]`: linked list of `a b c`
- `{k1 v1 k2 v2 ...}`: tree-map of `k1 -> v1`, `k2 -> v2`, etc
- `# stuff`: text comment (the space after `#` is required)
- `#[...]`, `#{...}`, or `#word`: structural comment
- `"foo"`: string (usual escapes are interpreted)
- `'x`: same as `[x]`
- `word`: word (some words are integers, some are floats, all are words)

Comma is a whitespace character, and `:` is whitespace if it immediately
follows a string. (This results in Canard being a syntactic superset of JSON.)
All whitespace is treated equivalently.

Words may contain parentheses, `#`, `'`, `"`, digits, and punctuation; a word
ends only at `[`, `]`, `{`, `}`, `,`, or whitespace.

## Lists
Lists are consed from left to right, just as they are in Lisp. Execution,
however, goes from right to left:

```
. [+ 3 5]               # produces 8
```

## Core functions
Name  | Arguments     | Effect    | Description
------|---------------|-----------|------------
`.`   | `[l]`         | `l`       | evaluate a list
`.`   | `{a b ...} a` | `b`       | evaluate a map
`?`   | `[t] [f] 0`   | `f`       | branch on truthiness
`?`   | `[t] [f] N`   | `t`       |
`::`  | `x [...]`     | `[x ...]` | cons
`:^`  | `[x ...]`     | `x [...]` | uncons

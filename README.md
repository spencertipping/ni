# ni: the concatenative streaming spreadsheet
![ni!](http://spencertipping.com/ni.png)

## Introduction
ni is to Excel what sed is to emacs. The spreadsheet metaphor mostly holds; the
only exceptions are:

1. No cell can refer downwards or rightwards, and
2. A cell can have multiple right neighbors (stacked vertically).

Here's an example. Suppose I want to calculate the average word length in
`/usr/share/dict/words`. In a normal spreadsheet I'd do something like this:

      | A            | B               | C
------|--------------|-----------------|--------------------
1     | A            | =length(A1)     | Here's the average:
2     | A's          | =length(A2)     | =average(B1:B99154)
3     | AA's         | =length(A3)     |
...   | ...          | ...             |
99154 | zygotes      | =length(A99154) |

In ni, cell C2 is invalid because it refers downwards; we need to move the
average to the bottom of the spreadsheet instead. It's also not a problem to
generate a running average instead of just the final, since that unifies the
formula in all C cells.

```sh
$ ni /usr/share/dict/words -x# -#a              # TODO
```

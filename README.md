# ni: the concatenative streaming spreadsheet
![ni!](http://spencertipping.com/ni.png)

## Introduction
ni is to Excel what sed is to emacs. The spreadsheet metaphor mostly holds; the
only exceptions are:

1. No cell can refer downwards or rightwards, and
2. A cell can have multiple right neighbors (stacked vertically).

Here's an example. Suppose I want to calculate the average word length in
`/usr/share/dict/words`. In a normal spreadsheet I'd do this (let's assume rule
1 applies from above):

      | A            | B               | C
------|--------------|-----------------|--------------------
1     | A            | =length(A1)     |
2     | A's          | =length(A2)     |
3     | AA's         | =length(A3)     |
...   | ...          | ...             |
99154 | zygotes      | =length(A99154) | =average(B1:B99154)

In ni, column A comes for free when you load a text file. If the text file
contained multiple tab-delimited columns we might start with more, but
`/usr/share/dict/words` has just one. We then add a column for the length, then
another one for the running average to that point:

```sh
$ ni /usr/share/dict/words \            # load the data
     --ruby 'A.size' \                  # create a new column for the lengths
     --average                          # vertical running average
```

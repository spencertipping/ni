# Joins
nfu had decent support for join operations, but ni can do better in a few
ways:

1. Provide left-random right-sorted joins via binary search. This eliminates
   the left-sort, which could save a lot of time.
2. Provide mixed joins: buffer a part of the left side, sort, then repeat.
   This makes it possible to join against an infinite stream given finite
   buffer space.
3. Provide prototyped joins: start by searching, then batch-sort as in (2).

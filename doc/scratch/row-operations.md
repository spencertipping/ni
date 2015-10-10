# Row operations and associated complexities
`-m` and `-A` are local operations: given that you've read row _i_, your
reference frame extends only to _i + 1_. `-g`, on the other hand, has unbounded
reference because you can't commit anything until you've seen everything.

## Limited-reference sorting
Should be possible; you just buffer a limited number of records, sort those,
emit them, and then sort the next batch. Any downstream processes can expect
discontinuities.

Let's table this. It's not necessarily a bad idea, but ni isn't a continuous
stream-processor (yet).

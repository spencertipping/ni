# ni for distributed workflows
Hadoop and Spark are great if you have them, but they're also
resource-intensive and nontrivial to set up. ni should provide a
resource-efficient way to do distributed computation on direct FS data, rather
than requiring something like HDFS or involving the JVM at all. ni should also
work with no setup other than having passwordless SSH access to a node.

**NB:** The following describes how ni thinks about distributed computation,
but you would never do it this way in practice. See
[metaprogramming](metaprogramming.md) for the abstractions to make the below
concise and usable.

## Concatenative resource streams
This is probably most easily explained by example:

```sh
# start with empty stream, then append the local file /usr/share/dict/words
$ ni /usr/share/dict/words

# append /usr/share/dict/words, take first 10k records
$ ni /usr/share/dict/words tE4

# append /usr/share/dict/words, fetched from other machine
# technically, "ssh into other-machine and run 'ni /usr/share/dict/words'"
$ ni other-machine:/usr/share/dict/words

# ssh into other-machine and run "ni /usr/share/dict/words tE4"
$ ni other-machine:[/usr/share/dict/words tE4]
```

So the map-side of a map/reduce workflow is just something like this:

```sh
$ ni machine1:[...] machine2:[...] machine3:[...] ...
```

Of course, if we ran that it would download all of the map results to the local
machine. That might be what we want, but it probably isn't. What we'd do
instead is to introduce a partitioning stage, in this case using the
deterministic-distribution `D` operator:

```sh
$ ni machine1:[mapper... D[machine1:[reducer] machine2:[reducer] ...]] \
     machine2:[mapper... D[machine1:[reducer] machine2:[reducer] ...]] \
     ...
```

This isn't a proper map/reduce workflow, however, because each mapper will
create however many of _its own_ reducer processes. No data is ever being
combined. Another consequence of this is that the reducers will send their
stream results back to the mappers, each of which just sends that same data
back to the original machine. In other words, we're not network-optimizing the
tail call to `D`.

## Named streams
What we really want is for the mapper/reducer connection to act kind of like a
data wormhole pipe:

```sh
# conceptually:
$ ni mapper:[some-data D[reducer:(write to pipe)]] (read from pipe)

# more concretely:
$ ni mapper:[some-data D[reducer:=pipe]] reducer:@pipe
```

Each pipe is a single-reader, single-writer object, just like a UNIX FIFO. So
if we want to do the usual map/reduce thing of combining map inputs, we need to
instruct the reducers to merge their sorted input pipes:

```sh
$ ni machine1:[mapper-stuff... D[[o machine1:=m1r1] [o machine2:=m1r2] ...]] \
     machine2:[mapper-stuff... D[[o machine1:=m2r1] [o machine2:=m2r2] ...]] \
     ... \
     machine1:[M[@m1r1 @m2r1 ... @mNr1] reducer-stuff...] \
     machine2:[M[@m1r2 @m2r2 ... @mNr2] reducer-stuff...] \
     ...
```

And the above logic is almost exactly how Hadoop's map/reduce handoff works,
including the map-side partitioned sort. Note that the sorts are all explicitly
specified; if you omit them you can do things like process infinite streams
using incremental map/reduce.

**TODO:** Can map/reduce stream topologies be encoded as matrix multiplication?
If so, the metaprogramming API gets a lot simpler.

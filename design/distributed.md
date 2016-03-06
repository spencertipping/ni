# ni for distributed workflows
Hadoop and Spark are great if you have them, but they're also
resource-intensive and nontrivial to set up. ni should provide a
resource-efficient way to do distributed computation on direct FS data, rather
than requiring something like HDFS or involving the JVM at all. ni should also
work with no setup other than having passwordless SSH access to a node.

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
$ ni machine1:[mapper... D0 [machine1:[reducer] machine2:[reducer] ...]] \
     machine2:[mapper... D0 [machine1:[reducer] machine2:[reducer] ...]] \
     ...
```

This isn't a proper map/reduce workflow, however, because each mapper will
create however many of _its own_ reducer processes. No data is ever being
combined. Another consequence of this is that the reducers will send their
stream results back to the mappers, each of which just sends that same data
back to the original machine. In other words, we're not optimizing the tail
call.



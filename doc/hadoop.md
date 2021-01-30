# Hadoop operator
```lazytest
# TODO: sequenceiq/hadoop is no longer active; I need to find a new image and
# probably update the hadoop streaming driver code to reflect any CLI changes
#
# This if condition is for lazytest; these tests are currently disabled.
if false; then
```

The `H` operator runs a Hadoop job. Here's what it looks like to use Hadoop
Streaming:

```sh
$ ni ihdfs:///input/path HS[mapper] [combiner] [reducer] > output-hdfs-path
```

For example, the ubiquitous word count:

```sh
# locally
$ ni README.md FW Z1 gcOx
ni      27
md      26
doc     25
com     16
spencertipping  12
https   11
to      9
the     9
github  9
and     9
...

# on hadoop streaming
$ ni ihdfs:///user/spencer/textfiles/part* HSFWZ1 _ c \<Ox
word1   count1
word2   count2
...
```

See [Hadoop in Ni By Example](ni_by_example_4.md#hadoop-streaming-mapreduce) for
a much better usage guide.

## Local setup
```sh
$ docker run --detach -i -m 2G --name ni-test-hadoop \
    sequenceiq/hadoop-docker \
    /etc/bootstrap.sh -bash
```

```lazytest
# unit test setup; you won't have to run this
if ! [[ $SKIP_DOCKER ]]; then
```

Let's start up a container and use `HS` to run a Streaming job. `H` is a
delegator, meaning that you always specify a profile (in this case `S` for
Streaming) before any command arguments.

(Note: The `until docker exec` silliness below is because we have to wait for
the hadoop container to boot up correctly. Sometimes the container gets into a
bad state and doesn't become available, in which case we nuke it and start
over. This is all just for unit testing; you won't have to worry about this
stuff if you're using ni to run hadoop jobs.)

```lazytest
# more unit test setup
start_time=0;
until docker exec -i ni-test-hadoop \
      /usr/local/hadoop/bin/hadoop fs -mkdir /test-dir; do
  if (( $(date +%s) - start_time > 60 )); then
    docker rm -f ni-test-hadoop >&2
    docker run --detach -i -m 2G --name ni-test-hadoop \
      sequenceiq/hadoop-docker \
      /etc/bootstrap.sh -bash >&2
    start_time=$(date +%s)
  fi
done
```

`S` takes three lambdas: the mapper, the combiner, and the reducer. There are
two special cases you can use instead of a normal bracketed lambda:

- `_`: skip this stage of the pipeline. If you use this for the combiner and
  reducer, then your job will be map-only.
- `:`: identity lambda. `HS:_:` is a simple way to repartition your input data,
  for example.

Hadoop input paths are specified using the input stream to a hadoop command. If
you specify something that doesn't start with `hdfs://` or `hdfst://`, the
stream will be uploaded to an HDFS tempfile and used as the job input. Notice
that we use `\<` after the hadoop command: `H` emits the output path, but not
the output data -- so if we want the data we need to read it.

```bash
$ NI_HADOOP=/usr/local/hadoop/bin/hadoop \
  ni n5 Eni-test-hadoop [HS[p'r a, a*a'] _ _ \<]
1	1
2	4
3	9
4	16
5	25
```

With a reducer:

```bash
$ ni n5 ^{hadoop/name=/usr/local/hadoop/bin/hadoop} \
          Eni-test-hadoop [HS[p'r a, a*a'] _ [p'r a, b+1'] \<] o
1	2
2	5
3	10
4	17
5	26
```


## Jobconf
You can pass in jobconf options using the `hadoop/jobconf` variable or by
setting `NI_HADOOP_JOBCONF` (note the different output; if you use multiple
reducers, you'll see the shard boundaries):

```bash
$ ni i'who let the dogs out who who who' \
     ^{hadoop/name=/usr/local/hadoop/bin/hadoop \
       hadoop/jobconf='mapred.map.tasks=10
       mapred.reduce.tasks=4'} \
     Eni-test-hadoop [HS[p'r a, a*a'] _ [p'r a, b+1'] \<] o
1	2
2	5
3	10
4	17
5	26
```

### Jobconf shorthand

The names of Hadoop configuration variables are generally quite long; to see the whole list, you can see the whole list in a Perl context using `%mr_generics`. Let's look at a few:

```bash
$ ni 1p'%mr_generics' Z2 e'grep memory' gA
Hcmm	mapreduce.cluster.mapmemory.mb
Hcrm	mapreduce.cluster.reducememory.mb
Hjtmmm	mapreduce.jobtracker.maxmapmemory.mb
Hjtmrm	mapreduce.jobtracker.maxreducememory.mb
Hmmm	mapreduce.map.memory.mb
Hrmm	mapreduce.reduce.memory.mb
Hrmt	mapreduce.reduce.memory.totalbytes
Htttm	mapreduce.tasktracker.taskmemorymanager.monitoringinterval
```

Use the abbreviations in the first column in your configuration; for example, to set your mappers to have 3072 MB of memory and reducers to have 4096 MB, you could do the following:

```bash
$ ni i'who let the dogs out who who who' \
     ^{hadoop/name=/usr/local/hadoop/bin/hadoop \
       Hrmm=4096 Hmmm=3072} \
     Eni-test-hadoop [HS[p'r a, a*a'] _ [p'r a, b+1'] \<] o
1	2
2	5
3	10
4	17
5	26
```


```lazytest
docker rm -f ni-test-hadoop >&2

fi                      # $SKIP_DOCKER (lazytest condition)

fi                      # if false (lazytest condition)
```

#Future Chapter 5 Below

##Data Development with `ni` and Hadoop

If you screw up your partitioning, you're ruined. 

JSON => Raw TSV => Cleaned TSV => Filtered TSV => Joined TSV => Statistics

If there's a potential problem with a column, you cannot use it as the first column in a reduce. If you do, Hadoop is very unforgiving; you'll end up having to bail on a particular slice of your data, or restart from the last nearly-equally-partitioned step.

Let's say you have data with 3 columns: `user_id`, `geohash`, `timestamp`. 

It's likely that both some of the users and some of the geohashes are spurious.  This will cause us difficulty down the line, for example when we want to join data to each record using geohash as the joining key.

### `:checkpoint_name[...]`: Checkpoints

Developing and deploying to producion `ni` pipelines that involve multiple Hadoop streaming steps involves a risk of failure outside the programmer's control; when this happens, we don't want to have to run the entire job again. 

Checkpoints allow you to save the results of difficult jobs while continuing to develop in `ni`.

Checkpoints and files share many commonalities. The key difference between a checkpoint and a file are:

1. A checkpoint will wait for an `EOF` before writing to the specified checkpoint name, so the data in a checkpoint file will consist of all the output of the operator it encloses. A file, on the other hand, will accept all output that is streamed in, with no guarantee that it is complete.
2. If you run a `ni` pipeline with checkpoints, and there are files that correspond to the checkpoint names in the directory from which `ni` is being run, then `ni` will use the data in the checkpoints in place of running the job. This allows you to save the work of long and expensive jobs, and to run these jobs multiple times without worrying that bhe steps earlier in the pipeline that have succeeded will have to be re-run.

An example of checkpoint use is the following:

```bash
$ ni nE6gr4 :numbers
1
10
100
1000
```

This computation will take a while, because `g` requires buffering to disk; However, this result has been checkpointed, thus more instuctions can be added to the command without requiring a complete re-run.


```bash
$ ni nE6gr4 :numbers O
1000
100
10
1
```

Compare this to the same code writing to a file:

```
$ ni nE6gr4 =\>numbers
1000
100
10
1
```

Because the sort is not checkpointed, adding to the command is not performant, and everything must be recomputed.

```
$ ni nE6gr4 =\>numbers O
1000
100
10
1
```

One caveat with checkpoint files is that they are persisted, so these files must be cleared between separate runs of `ni` pipelines to avoid collisions. Luckily, if you're using checkpoints to do your data science, errors like these will come out fast and should be obvious.


### Developing Hadoop Streaming pipelines with checkpoints

As of now, `ni` auto-generates the names for the Hadoop directories, and these can be hard to remember off the top of your head. If you want to 


###Tools for Data Development

1. A good markdown editor; I like Laverna, and it should work on basically all platforms.
2. Infinite patience
3. A reasonable test set.


##Plotting with `ni --js`
Check out the [tutorial](tutorial.md) for some examples of cool, interactive `ni` plotting.

**TODO**: Say something useful.


##Custom Compound Reduce
#### `rfn`: Custom compound reduce

**TODO: Understand this**

##Partitioned Matrix Operations

Operations on huge matrices are not entirely `ni`ic, since they may require space greater than memory, whichwill make them slow. However, operators are provided to improve These operations are suited best to 


* `X<col>`, `Y<col>`, `N<col>`: Matrix Partitioning
  * **TODO**: understand how these actually work.
* `X`: sparse-to-dense transformation
  * In the case that there are collisions for locations `X`, `X` will sum the values
  * For example: `ni n010p'r 0, a%3, 1' X`

##Disk-Backed Data Closures

* `@:[disk_backed_data_closure]`

##Binary Operations
In theory, this can save you a lot of space. But I haven't used this in practice.

##Less Useful `ni`-specific Perl Extensions


###Array Functions
  * `clip`
  * `within`
  

   
##Writing Your Own `ni` Extensions
**TODO** Understand how this works

##Obscure Interops/Extensions

* [SQL](sql.md)
* [PySpark](pyspark.md)
* [Scripting](script.md)
* [HTTP Operations](net.md)
* [Defining `ni` functions](fn.md)
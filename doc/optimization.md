#Performance Optimization

##Hadoop Streaming
###Jobs are not generating enough splits
If you're using reasonably simple mapper/reducer/combiner operations, on a reasonably-not-huge chunk of data, the main reason your jobs will be slow is that they are not properly distributed. It's difficult to control the number of map jobs (Hadoop often has its own ideas about what is the right number of maps), but you can control the number of files that will be reduced using `export NI_HADOOP_JOBCONF="mapreduce.job.reduces=1024"`

Every downstream operation Hadoop Streaming job of the one with that many reducers should use a large number of mappers and have strong HDFS I/O.

###Job should work but it doesn't start
If you're modifying `ni` with a data closure, it may be that the closure is making the `jar` `ni` is packing itself into too large for the Hadoop job server to accept. Try slimming down the size of the jar as much as possible; consider using `,z` to hash long strings to shorter integers.

Also, remember that the Hadoop job (probably) does not have access to files on the machine from which it was called; this may cause errors that are somewhat difficult to read.


##Reading from HDFS is slow
There are several commands to read from HDFS; be sure that you are using the appropriate one.

###Using HDFS in a Hadoop Streaming job
When running a Hadoop streaming job, you should use a **quoted path**. For example, `ni \'hdfst:///path/to/file/part-* HS:_:` will re-partition your data on HDFS.

###Using HDFS outside of Hadoop Streaming
To read HDFS into the input stream, you'll want to use `ni`'s `hdfst` (hadoop file system *text*) operator to get data out, supporting the bash-style use of `*`.


##Sort is slow
Sorting a large amount of data is going to be a slow operation on any machine, period, because the data is going to need to be buffered at some point. However, sorting on a Mac can be very slow, probably because the `gnu-coreutils` for Mac are slow.  To remedy this, install Docker and use a containerized sort. 

`ni n1E7 g` runs in about 80 seconds on a Late 2013 2.6 GHz Intel Core i5, whereas

`ni n1E7 Cubuntu[g]` **TODO**: find the example where this actually work>
 
If you're trying to sort gigabytes (or more) of data, you should consider rewriting your workflow to use Hadoop operations (if you have access to a cluster)

##Math is hard to write and slow
Math operations that would consume the whole stream at once are going to be a little awkward using `ni` stream operators. Use `N'...'` to access a numpy environment instead.

##Unusable Operators
As of 2016-12-18, the `v` vertical processing operator is too slow to use.
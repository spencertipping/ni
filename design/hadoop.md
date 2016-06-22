# Hadoop interop
There are a lot of workflows that have mixed distributed/local processing. For
example, a process to extract data on Hadoop (e.g. a sparse query) followed by
local aggregation. We'll probably want checkpointing to apply to both cases.

```sh
$ ni hdfs:/data/... h[sparse-query] gcxopL
```

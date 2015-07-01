## Simple stream manipulation
ni behaves like a smarter version of `less` if you specify no transformation
operators:

```sh
$ ni                            # pipe from stdin to stdout
$ ni file1 file2                # previews contents with 'less'
$ ni file1.gz file2.bz2         # automatically decompresses stuff
$ ni http://google.com          # download HTTP
$ ni me@host:file.gz            # transfers over SSH
$ ni hdfs:/path/to/data.gz      # retrieves over HDFS
$ ni s3://foo/bar/bif           # retrieves with s3cmd
$ ni sql3:/tmp/mydb.db:foo      # convert SQLite 'foo' table to TSV
$ ni sql3::'select * from foo'  # query results from ${TMPDIR:-/tmp}/$USER.db
$ ni psql:mydb:foo              # ditto, but PostgreSQL
$ ni lucene:/path               # all documents from lucene index
$ ni lucene:/path:'{"foo":"bar"}'
$ ni fd:3                       # stream data from file descriptor 3
$ ni perl:'0..99'               # stream from perl
$ ni ruby:'(0...100).map {|x| x + 1}'
$ ni python:'range(50)'
$ ni sh:'find . -name foo'      # stream from shell command stdout
$ ni n:100                      # numbers from 0 to 99 inclusive
```

All ni commands start with `-` for short options or `--` for long ones:

```sh
$ ni n:100 -X shuf -t1          # choose random number in [0, 99]
$ ni n:100 --shell shuf --take 1
```

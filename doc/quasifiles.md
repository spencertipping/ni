# Quasifile reference
## URL-syntax quasifiles
Short   | Long          | Format          | Description
--------|---------------|-----------------|------------
`b:`    | `bloom:`      | `b:/path`       | Bloom filter
`c:`    | `countmin:`   | `c:/path`       | Count-min sketch
`f:`    | `file:`       | `f:/path`       | Local file
`//`    | `http[s]://`  | `//google.com`  | HTTP url (retrieved with curl)
`h:`    | `hll:`        | `h:/path`       | HyperLogLog
`n:`    |               | `n:[l,[s,]]u`   | Numeric range
`p:`    | `gnuplot:`    | `p:...`         | Gnuplot
`q:`    | `sqlite:`     | `q:[db:]t`      | SQLite table
`Q:`    | `psql:`       | `Q:[db:]t`      | PostgreSQL table
`r:`    | `redis:`      | `r:host`        | Redis database
`s:`    | `ssh:`        | `s:[u@]h:qfile` | SSH tunnel
`s3:`   | `s3://`       | `s3:path`       | S3 url (retrieved with s3cmd)
`t:`    | `text:`       | `t:text`        | Literal constant text
`$:`    | `sh:`         | `$:command`     | Shell command stdout

## Archives
ni uses magic numbers to determine whether input data is one of a few standard
compressed or archive formats, each of which is identified by its magic numbers
but manipulated with system commands that ni assumes are already installed:

- ar
- gzip
- bzip2
- lzo (`lzop` command)
- lz4
- lzip
- pkzip/jar (`unzip` command)
- tar
- xz

pkzip, tar, and directories behave identically: if used directly, they resolve
to qualified children, and all support use within a path. Compressed files are
automatically decompressed.

Although ni will unpack any of the above archive formats given the right system
tools, it won't generate them by itself.

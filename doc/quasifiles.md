# Quasifile reference
Quasifile prefixes are designed to minimize ergonomic overhead when typed on a
QWERTY keyboard. When writing prefixes, `.` and `:` are interchangeable; `.` is
the default in documentation because it doesn't involve the shift key.

## URL-syntax quasifiles
Short   | Long          | Format          | Description
--------|---------------|-----------------|------------
`b.`    | `bloom.`      | `b./path`       | Bloom filter
`f.`    | `file.`       | `f./path`       | Local file
`g.`    | `gnuplot.`    | `g....`         | Gnuplot
`k.`    |               | `k.text`        | Literal constant text
`//`    | `http[s].//`  | `//google.com`  | HTTP url (retrieved with curl)
`n.`    |               | `n.[l,[s,]]u`   | Numeric range
`r.`    | `redis.`      | `r.host`        | Redis database
`s.`    | `ssh.`        | `s.[u@]h:qfile` | SSH tunnel
`s3.`   | `s3.//`       | `s3.path`       | S3 url (retrieved with s3cmd)
`$.`    | `sh.`         | `$.command`     | Shell command stdout

## Archives
ni uses magic numbers to determine whether input data is one of a few standard
compressed or archive formats:

- gzip
- bzip2
- lzo
- lz4
- snappy
- pkzip/jar/odX
- tar
- xz

pkzip, tar, and directories behave identically: if used directly, they resolve
to qualified children, and all support use within a path. Compressed files are
automatically decompressed.

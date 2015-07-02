# Quasifile reference
Quasifile prefixes are designed to minimize ergonomic overhead when typed on a
QWERTY keyboard.

## URL-syntax quasifiles
Short   | Long          | Format          | Description
--------|---------------|-----------------|------------
`b:`    | `bloom:`      | `b:/path`       | Bloom filter
`f:`    | `file:`       | `f:/path`       | Local file
`g:`    | `gnuplot:`    | `g:...`         | Gnuplot
`k:`    |               | `k:text`        | Literal constant text
`//`    | `http[s]://`  | `//google.com`  | HTTP url (retrieved with curl)
`n:`    |               | `n:[l,[s,]]u`   | Numeric range
`r:`    | `redis:`      | `r:host`        | Redis database
`s:`    | `ssh:`        | `s:[u@]h:qfile` | SSH tunnel
`s3:`   | `s3://`       | `s3:path`       | S3 url (retrieved with s3cmd)
`$:`    | `sh:`         | `$:command`     | Shell command stdout

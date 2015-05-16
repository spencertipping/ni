## Optional dependencies
ni has no core dependencies beyond libc and a C compiler, but certain features
won't work unless command-line tools exist on your `$PATH`:

- PostgreSQL IO requires the `psql` executable
- SQLite IO requires the `sqlite3` executable
- Lucene and other Java integration requires the `mvn` executable and a working
  Internet connection; it assumes Java v5 or later and Maven 2 or later
- S3 access requires `s3cmd`
- HTTP GET/PUT requires `curl`
- Paging requires either `less` or `more`, defaulting to `less`
- Remoting requires `ssh`
- Transparent decompression requires relevant executables (`gz`, `lzop`,
  `bzip2`, `xz`)

In addition, programming-language integrations require the relevant
executables:

- Ruby requires `ruby` version 1.8.7 or greater
- Python requires `python`, version 2.4 or greater
- Perl requires `perl`, version 5.0 or greater
- R requires `R`, version 3 or greater
- C requires `c99`
- Java requires `javac` version 5 or greater, and `mvn` version 2 or greater
- Clojure requires `lein`, version 2 or greater
- Scala requires `scala`, version 2 or greater

ni generates wrapper code for each of these environments, and does so in a way
that maximizes version/library compatibility. This means, for example, that you
can use Ruby 2 and get lazy iterators; but 1.8.7 will also work with standard
`Enumerator` instances. ni's adapter code will work with either version.

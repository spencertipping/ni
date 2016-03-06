- General
  - Full portability
  - Spreadsheet-style text processing
  - Perl text processing (not spreadsheet-style)

- IO
  - Hadoop `InputFormat`/`OutputFormat` encoding/decoding
  - UNIX file format encoding/decoding (decoding, mostly; encoding is usually
    trivial)
  - Data retrieval from various places (http, ssh, etc: the stuff nfu supports
    + archives + directories)
  - Browser-based plotting (much faster, more portable, and more functional
    than gnuplot)

- Interop
  - Most CLI options expand to UNIX shell commands, or JIT C programs
  - Make JVM easier to use
  - Trivially interop with Perl, Python, Ruby, Octave, R: handle IO and data
    conversion

- Parallelism
  - UNIX-level distributed computation, possibly with filters like
    ssh-distribution, etc (think about the operation set here)
  - Hadoop interop (HDFS up/down + streaming, possibly Java code wrapping)
  - Spark interop (HDFS up/down + jobs)
  - Trivial resource serialization: some syntax that auto-bundles/broadcasts a
    stream into a file

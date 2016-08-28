# ni: portable, self-modifying APL for unstructured data
![ni!](http://spencertipping.com/ni.png)

```sh
$ ni --help                     # start here
```

## APL, really?
Not really, but conceptually similar. Ni is optimized to require as few
characters as possible; for example, the ubiquitous word count, sorting by
descending word frequency:

```sh
$ ni README.md FWpF_ CO         # run on POSIX tools locally
$ ni README.md hFWpF_ cO        # run on hadoop streaming
$ ni README.md PL[FWpF_ CO]     # run on pyspark
```

- `FW`: fieldsplit on non-words
- `pF_`: map with Perl code, in this case `F_`, which is the array of fields
  (this transposes horizontal rows to vertical)
- `C`: sort+count (hadoop version uses `c` instead because the input is already
  sorted)
- `O`: reverse sort numerically

Most operators are a single character, and ni maintains a consistent data
abstraction of "everything's a spreadsheet."

## WTF???
Glad you asked, because ni is self-documenting:

```sh
$ ni //help                     # ni --help also works
$ ni //help/topic
$ ni --explain [options...]     # TODO
$ ni --compile [options...]     # produces POSIX shell script
```

## Dev scripts and testing
- `./build`: create `ni` from source
- `./test`: rebuild and run documentation tests for `ni`

Documentation tests are fenced code snippets from the markdown docs - `sh` code
is ignored, but `bash` code gets turned into test cases. It's written in
transcript style; that is, you're demonstrating a command and its output:

```sh
$ echo hi               # when I run this
hi                      # I should get this
```

Each such command/output pair becomes an assertion. See
`scripts/transcript-gen` for the parser and `scripts/transcript-verify` for the
logic that runs these assertions.

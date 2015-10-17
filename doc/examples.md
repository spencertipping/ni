# Examples
A bunch of ways you might use ni to go about data manipulation tasks.

## Map/reduce word count
```sh
$ ni data.txt -FW1k1gm'r f0, a0i1.sum'                  # local
$ ni hdfs:data.txt -hFW1k1/^m'r f0, a0i1.sum'           # local
$ ni hdfs:data.txt -HFW1k1/^m'r f0, a0i1.sum'           # hadoop
```

- `-FW`: shorthand for `-F '\W+'`: split on non-words
- `-1k1`: address `-k1` to field 1, which juxtaposes each word with 1
- `-g`: group
- `-m`: execute ruby code on each row
    - `r x, y`: emit a row of values
    - `f0`: value of first field
    - `a0i1`: integer interpretation of field 1 forward-aggregated by field 0

Using the `-A/--aggregate` operator:

```sh
$ ni data.txt -FW1k1gA^1st+1
$ ni hdfs:data.txt -HFW1k1/A^1st+1
```

Non-MR word count (using command-line sort):

```sh
$ ni data.txt -FWvgc                                    # prepend count
$ ni data.txt -FWvgcx                                   # append count
```

## Index JSON dataset by geohash
```sh
$ ni data -m'r j0.name, ge(j0.latitude, j0.longitude, 8)' -xg
```

`j0` means "`f0` interpreted as JSON". It notationally bypasses the caching
otherwise necessary to support multiple-access. `ge` is a builtin function to
encode geohashes.

When performance is important, you're probably better off using ni's JSON
parser generator like this:

```sh
$ ni data -Jname,latitude,longitude -m'r s0, ge(d1, d2, 8)' -xg
```

## Record extraction
Variants of `-t` allow you to extract lines matching a specific pattern,
internally using `grep -E`:

```sh
$ ni hdfs:/data/source -Ht/foo.*bar //=output.gz
```

If you don't need partial results, it's often easier to use checkpoint files
with `:`, rather than the destructive-output `=` operator:

```sh
$ ni hdfs:/data/source -Ht/foo.*bar //:output.gz
```

The advantage is that you can just append stuff to the previous command and
your output will be reused.

## Feature matrix construction
Doing this correctly requires buffering because we're building a numerical
index. For example, let's suppose we have a bunch of JSON arrays of city names,
and the goal is to convert those to a matrix of row/city frequency. We also
want to export the label mapping as `./labels` for debugging purposes.

```sh
$ ni data -J* @=labels^vG1n -m'ls = (1..labels.size).map {0}
                               ss.each {|s| ls[labels[s] - 1] += 1}
                               r ls'
```

- `-J*`: unpack JSON array to a row of values
- `@=`: create both a variable and a file (`@` = variable, `=` = file)
- `^vG1n`: modifier lambda: vertical, groupuniq, line number -> column 1
  (ni converts this to a hashmap when you define a variable)

## Checkpointing
Some workflow stages are slow. If you want to avoid repeating these, use a
checkpoint file:

```sh
$ ni hdfs:/data/source -t1E5 ...                # slow
$ ni hdfs:/data/source -t1E5 :input.gz          # fast for subsequent runs
$ ni :input.gz[ hdfs:/data/source -t1E5 ] ...   # same thing, more explicit
```

Note that checkpointing can't be used with `@`.

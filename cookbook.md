# `ni` cookbook
An ongoing collection of real-world tasks I use `ni` to solve.


## System administration
I've used `ni` for a lot of miscellaneous sysadmin tasks, especially things
involving bulk file renames, analysis, or log processing. For destructive
actions I'll start by staging everything as data, then finalizing the stream
with a simple mapper that applies the transformation. For example:

```sh
$ ni ...                  # produces "oldname newname" rows as data
$ ni ... p'rename a, b'   # consumes those rows and applies the side effect
```

**NOTE:** be sure to use `G` to seek to the bottom of your pager if you use a
Perl mapper to cause a side effect. If you don't, it's possible the mapping
process will be blocked before emitting all output. You can alternatively append
`zn` or `wcl` to consume all data without streaming it into a pager, especially
if you don't care about individual operation return codes:

```sh
$ ni ... p'rename a, b' zn
```


### Where's my disk usage?
```sh
$ ni edu O                # disk usage from here
$ ni e[du -x /] O         # disk usage on /, don't descend into mountpoints

$ du | sort -rn | less    # equivalent longhand
```

Without arguments, `du` outputs lines of the form `size path`, where `size` is a
consistent unit like blocks or KB. Paths are explored recursively. Using `O` to
reverse-sort numerically yields a top-down list of largest directories.

Since `du` can take a while sometimes, I'll often run it overnight and save the
results for later:

```sh
$ ni edu O z\>du-results
$ ni du-results ...
```


### Remove awkward characters from filenames
```sh
$ ni . p'r a, a =~ s/\W+/-/gr' p'rename a, b' zn
```

This is an operation where previewing and iteration are especially important.
You usually want to make sure that (1) your resulting filenames look reasonable,
and (2) none of them collide. Here's my usual strategy, starting with a stream
of filenames:

```sh
$ ni .                    # 'ni' on directories == ls -a
$ ni e[find -type f]      # on linux
$ ni e[find . -type f]    # OSX requires '.' for find
```

This produces a stream with the original filename on each line. We want a stream
of `oldname newname`, so we use a Perl mapper to transform it:

```sh
$ ni ... p'r a, a =~ s/\W+/-/gr'                      # clobber extensions
$ ni ... p'r a, a =~ s/\W+/-/gr =~ s/-(\w+)$/\.$1/r'  # keep extensions

# this is what I often use:
$ ni ... p'r a, lc(a) =~ s/\W+/-/gr =~ s/(.)-(\w+)$/$1.$2/r =~ s/^-//r'
```

My version above isn't perfect; if I have extensionless files with hyphenated
names, it will turn the last part into an extension. In cases like that it can
help to pre-filter rows so we don't worry about filenames that are already in
good shape:

```sh
$ ni ... r'/[^-\w\/\.]/'
```

I mentioned checking for duplicate destinations; here's a simple way to do that:

```sh
$ ni ... fBgcrp'a > 1'
```

+ `fB` selects just the destinations (column `B`)
+ `gc` sorts and then counts, just like `sort | uniq -c`
+ `rp'a > 1'` returns rows whose count is greater than 1, i.e. it isn't unique

# ni: the concatenative streaming spreadsheet that says "ni" to data
![ni!](http://spencertipping.com/ni.png)

ni is a self-modifying data pipeline constructor whose command-line syntax is
a mix of Joy and APL, though most of your code for it is likely to be Ruby,
Python+Numpy, Perl, R, Caterwaul, or Octave. If you are a data scientist, ni
will change your life.

## Using ni
If you have no idea what to do with ni yet, you can start by using it as an
enhanced version of `less`:

```sh
$ ni /usr/share/dict/words              # same as less /usr/share/dict/words
$ ni data.gz                            # automatically decompresses it
$ ni @host:data                         # automatically forwards over SSH
$ ni foo.zip                            # list files in the archive
$ ni foo.zip/bar                        # cat one of the files
```

I recommend reading [the documentation](doc/), which is also the test suite
(so each example behaves exactly as described).

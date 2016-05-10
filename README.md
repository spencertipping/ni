# ni: the concatenative streaming spreadsheet
![ni!](http://spencertipping.com/ni.png)

ni is a self-modifying data pipeline constructor whose command-line syntax is
a mix of Joy and APL. It's written in Perl, but uses POSIX sh as a backend for
portability. If you are a data scientist, ni will change your life.

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

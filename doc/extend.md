# Extending ni
You can extend ni by writing a library. For example, suppose we want a new
operator `N` that counts lines by shelling out to `wc -l`:

```bash
$ mkdir my-library
$ echo my-lib.pl > my-library/lib
$ echo "defshort 'root', 'N', k sh ['wc', '-l'];" > my-library/my-lib.pl
$ ni --lib my-library n:100N
100
```

## Libraries
A library is just a directory with a `lib` file in it. `lib` lists the names of
files to be included within that library, and in doing so specifies the order
of inclusion (which sometimes matters if you're defining stuff in Perl).

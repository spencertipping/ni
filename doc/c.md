# C code mobility
ni provides an internal function, `exec_c99`, which you can call from within an
operator to replace your perl runtime with a C99 runtime (this assumes the host
machine has a `c99` compiler, which is very common and specified by POSIX 2001
CD).

It's up to you to write a full C program capable of consuming stdin and writing
to stdout; for example:

```bash
$ cat > wcl.pl <<'EOF'
# Defines the "wcl" operator, which works like "wc -l"
defoperator wcl => q{
  exec_c99 indent(q{
    #include <unistd.h>
    #include <stdio.h>
    int main(int argc, char **argv)
    {
      char buf[8192];
      ssize_t got = 0;
      long lines = 0;
      unlink(argv[0]);
      while (got = read(0, buf, sizeof(buf)))
        while (--got)
          lines += buf[got] == '\n';
      printf("%ld\n", lines);
      return 0;
    }
  }, -4);
};

defshort '/wcl' => pmap q{wcl_op}, pnone;
EOF
```

Now let's use that operator:

```bash
$ ni --lib wcl.pl n10 wcl
10
```

**NOTE:** The interfacing between ni and C is minimal, but ni assumes that your
program will unlink itself once it's running. Otherwise you'll leave tempfiles
behind. Here's how you should do this:

```c
#include <unistd.h>
int main(int argc, char **argv)
{
  unlink(argv[0]);
  /* the rest of your code */
}
```

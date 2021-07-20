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


## `c99` operator
ni defines the `c99''` operator to JIT C source into a temporary executable,
then use that executable as a stream filter. For example, to calculate the
frequency of each input byte efficiently:

```bash
$ ni n100 c99'#include <stdint.h>
              #include <stdlib.h>
              #include <stdio.h>
              #include <unistd.h>
              int main(int argc, char **argv)
              {
                uint64_t c[256] = {0};
                unsigned char buf[65536];
                int n;
                unlink(argv[0]);
                while (n = read(0, buf, sizeof(buf)))
                  for (int i = 0; i < n; ++i)
                    ++c[buf[i]];
                for (int i = 0; i < 256; ++i)
                  printf("%d\t%ld\n", i, c[i]);
                return 0;
              }' rpb p'r je chr(a), b'
"\n"    100
0       11
1       21
2       20
3       20
4       20
5       20
6       20
7       20
8       20
9       20
```

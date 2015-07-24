# What's going on here
ni is a native program that gets compiled by a surrounding shell script. Here's
how that works:

```sh
#!/bin/sh
cat > sourcefile.c <<EOF
// ni source code as C
EOF
c99 sourcefile.c -o ni-binary && exec ni-binary
```

There's some stuff going on to manage tempfiles, etc, but that's the general
idea. However, if that were all we were doing, ni would have no way to send a
copy of itself across SSH to be rebuilt on the host. So we modify the above
slightly to give the C program enough information to reproduce the whole shell
script.

```sh
#!/bin/sh
awk 'decompressor-program' > sourcefile.c <<EOF
12 decompressor-program
<decompressor-program verbatim, 12 lines>
180 ni.c
<ni.c verbatim, 180 lines>
...
EOF
c99 sourcefile.c -o ni-binary && exec ni-binary
```

## The decompressor
The decompressor does two things. First, it parses a very simple resource
stream format described in [sh/decompress.awk.sdoc](sh/decompress.awk.sdoc). In
doing so, it emits quoted C strings containing that data.

Second, it keeps track of which resources end in `.c` and emits those verbatim
after all of the quoted stuff. It also emits some indexes to access the
generated C strings. Here's a simple resource stream:

```
2 header.sh
#!/bin/sh
cat > sourcefile.c <<EOF
2 footer.sh
EOF
c99 sourcefile.c -o ni-binary && exec ni-binary
3 main.c
int main() {
  return 0;
}
```

And here's the C code that would be produced by `sh/decompress.awk` (with some
formatting and comments so it's easier to read):

```c
/* quoted header.sh */
static const char *const qheader_sh[] = {
  "#!/bin/sh\n",
  "cat > sourcefile.c <<EOF\n",
  0
};

/* quoted footer.sh */
static const char *const qfooter_sh[] = {
  "EOF\n",
  "c99 sourcefile.c -o ni-binary && exec ni-binary\n",
  0
};

/* quoted main.c */
static const char *const qmain_c[] = {
  "int main() {\n",
  "  return 0;\n",
  "}\n",
  0
};

/* list of resource names */
static char const *const rn[] = {
  "header.sh",
  "footer.sh",
  "main.c",
  0
};

/* list of resources, in the same order as rn[] above */
static char const *const *const rs[] = {
  qheader_sh,
  qfooter_sh,
  qmain_c,
  0
};

/* unquoted C source */
int main() {
  return 0;
}
```

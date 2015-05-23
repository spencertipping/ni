#!/bin/sh
# ni self-compiling source image; not intended to be edited directly
# MIT license, see https://github.com/spencertipping/ni for details
e=`mktemp`
s=`mktemp --suffix=.c`
{
awk '{
  if (!ls--) {
    if (r) print "(const char *const) 0};"
    interp = (rs[rn++] = r = $2) ~ /\.c$/
    ls = $1
    if (r) print "static const char *const q" (rn - 1) "[] = {"
  } else {
    if (interp) {code[c++] = $0}
    gsub("\\\\", "\\\\\\\\")
    gsub("\"", "\\\"")
    print "\"" $0 "\","
  }
}
END {
  if (r) print "(const char *const) 0};"
  print "static const char *const rn[] = {"
  for (i = 0; i < rn; ++i) print "\"" rs[i] "\","
  print "(const char *const) 0};"
  print "static const char *const *const rs[] = {"
  for (i = 0; i < rn; ++i) print "q" i ","
  print "(const char *const *const) 0};"
  for (i = 0; i < c; ++i) print code[i]
}' <<'EOF'
23 decompress.awk
{
  if (!ls--) {
    if (r) print "(const char *const) 0};"
    interp = (rs[rn++] = r = $2) ~ /\.c$/
    ls = $1
    if (r) print "static const char *const q" (rn - 1) "[] = {"
  } else {
    if (interp) {code[c++] = $0}
    gsub("\\\\", "\\\\\\\\")
    gsub("\"", "\\\"")
    print "\"" $0 "\","
  }
}
END {
  if (r) print "(const char *const) 0};"
  print "static const char *const rn[] = {"
  for (i = 0; i < rn; ++i) print "\"" rs[i] "\","
  print "(const char *const) 0};"
  print "static const char *const *const rs[] = {"
  for (i = 0; i < rn; ++i) print "q" i ","
  print "(const char *const *const) 0};"
  for (i = 0; i < c; ++i) print code[i]
}
27 ni.c
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#define EXIT_NORMAL       0
#define EXIT_USER_ERROR   1
#define EXIT_SYSTEM_ERROR 2
void usage(void) {
  fprintf(stderr, "TODO: usage\n");
}
#define die(...) \
  do { \
    fprintf(stderr, "ni: " __VA_ARGS__); \
    exit(EXIT_SYSTEM_ERROR); \
  } while (0);
int main(const int argc, const char *const *argv) {
  if (unlink(argv[0])) die("unlink failed for %s", argv[0]);
  if (unlink(argv[1])) die("unlink failed for %s", argv[1]);
  const int stdin_tty = isatty(STDIN_FILENO);
  if (argc == 2 && stdin_tty) {
    usage();
    return EXIT_USER_ERROR;
  }
  for (int i = 0; rs[i]; ++i)
    for (int j = 0; rs[i][j]; ++j)
      printf("%s:%d: %s\n", rn[i], j + 1, rs[i][j]);
  return EXIT_NORMAL;
}
6 ni-header.sh
#!/bin/sh
# ni self-compiling source image; not intended to be edited directly
# MIT license, see https://github.com/spencertipping/ni for details
e=`mktemp`
s=`mktemp --suffix=.c`
{
2 ni-footer.sh
} > "$s"
c99 "$s" -o "$e" && exec "$e" "$s" "$@"
EOF
} > "$s"
c99 "$s" -o "$e" && exec "$e" "$s" "$@"

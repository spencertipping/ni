#!/bin/sh
# ni self-compiling source image; not intended to be edited directly
# MIT license, see https://github.com/spencertipping/ni for details
prefix=${TMPDIR:-/tmp}/ni-$USER-$$
i=0
until mkdir "$prefix-$i" 2>&1 > /dev/null; do
  i=`expr $i + 1`
done
e=$prefix-$i/ni
s=$e.c
{
awk '{
  if (!ls--) {
    if (r) print "(const char *const) 0};"
    interp = (rs[rn++] = r = $2) ~ /\.c$/
    ra[r] = "q" gensub("\\W", "_", "g", r)
    ls = $1
    if (r) print "static const char *const " ra[r] "[] = {"
  } else {
    if (interp) code[c++] = $0
    gsub("\\\\", "\\\\")
    gsub("\"", "\\\"")
    print "\"" $0 "\\n\","
  }
}
END {
  if (r) print "(char const *const) 0};"
  print "static char const *const rn[] = {"
  for (i = 0; i < rn; ++i) print "\"" rs[i] "\","
  print "(char const *const) 0};"
  print "static char const *const *const rs[] = {"
  for (i = 0; i < rn; ++i) print ra[rs[i]] ","
  print "(char const *const *const) 0};"
  for (i = 0; i < c; ++i) print code[i]
}
' <<'EOF'
24 decompress.awk
{
  if (!ls--) {
    if (r) print "(const char *const) 0};"
    interp = (rs[rn++] = r = $2) ~ /\.c$/
    ra[r] = "q" gensub("\\W", "_", "g", r)
    ls = $1
    if (r) print "static const char *const " ra[r] "[] = {"
  } else {
    if (interp) code[c++] = $0
    gsub("\\\\", "\\\\")
    gsub("\"", "\\\"")
    print "\"" $0 "\\n\","
  }
}
END {
  if (r) print "(char const *const) 0};"
  print "static char const *const rn[] = {"
  for (i = 0; i < rn; ++i) print "\"" rs[i] "\","
  print "(char const *const) 0};"
  print "static char const *const *const rs[] = {"
  for (i = 0; i < rn; ++i) print ra[rs[i]] ","
  print "(char const *const *const) 0};"
  for (i = 0; i < c; ++i) print code[i]
}
121 ni.c
#define EXIT_NORMAL       0
#define EXIT_SYSTEM_ERROR 2
#define EXIT_USER_ERROR   1
#define _ISOC99_SOURCE
#define NI_ASSERT_NOPE 2
#define NI_LIMIT_NOPE  1
#define NI_SYSTEM_ERROR  2
#define NI_THIS_IS_A_BUG 3
#define NI_USER_ERROR    1
#define _POSIX_C_SOURCE 200112L
#define _XOPEN_SOURCE   600
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
void ni_nope_exit(int const reason) {
  switch (reason) {
    case NI_LIMIT_NOPE:
      fprintf(stderr, "exiting with code %d due to exceeded limit\n", reason);
      exit(NI_USER_ERROR);
      break;
    case NI_ASSERT_NOPE:
      fprintf(stderr, "exiting with code %d due to failed assert\n", reason);
      exit(NI_THIS_IS_A_BUG);
      break;
    default:
      fprintf(stderr, "exiting for unknown reason (ni bug): %d\n", reason);
      exit(NI_THIS_IS_A_BUG);
      break;
  }
}
#define ni_assert_nope(cond, ...)                                       \
  do {                                                                  \
    if (!(cond)) {                                                      \
      fprintf(stderr, __VA_ARGS__);                                     \
      fprintf(stderr, "(this is a ni bug; sorry about this)\n");        \
      ni_nope_exit(NI_ASSERT_NOPE);                                     \
    }                                                                   \
  } while (0)
#define ni_limit_nope(val, limit, ...)                                  \
  do {                                                                  \
    uint64_t nope_val = (val);                                          \
    while (nope_val > limit) {                                          \
      fprintf(stderr, __VA_ARGS__);                                     \
      fprintf(stderr, "\n"                                              \
                      "- 'n' to exit ni\n"                              \
                      "- a new number to change the limit\n"            \
                      "- 'y' to increase the limit and Just Work\n"     \
                      "> ");                                            \
      fflush(stderr);                                                   \
      unsigned long long nope_new_limit;                                \
      if (fscanf(stderr, "%llu", &nope_new_limit))                      \
        limit = nope_new_limit;                                         \
      else {                                                            \
        char nope_reply;                                                \
        fscanf(stderr, "%c", &nope_reply);                              \
        switch (nope_reply) {                                           \
          case 'n':                                                     \
            ni_nope_exit(NI_LIMIT_NOPE);                                \
            break;                                                      \
          case 'y':                                                     \
            limit = nope_val;                                           \
            break;                                                      \
        }                                                               \
      }                                                                 \
    }                                                                   \
  } while (0)
#define ni_ull(x) ((unsigned long long) (x))
int ni_intlog2(uint64_t x)
{
  uint64_t y;
  int l = 0;
  for (int i = 5; i >= 0; --i) {
    int shift = 1 << i;
    if (y = x >> shift) {
      x = y;
      l += shift;
    }
  }
  return l;
}
int ni_cintlog2(uint64_t x)
{
  int log = ni_intlog2(x);
  if (1 << log < x) ++log;
  return log;
}
#define for_rs_names(i)       for (int i = 0; rs[i]; ++i)
#define for_rs_parts(name, i) for (int i = 0; name[i]; ++i)
#define die(...) \
  do { \
    fprintf(stderr, "ni: " __VA_ARGS__); \
    exit(EXIT_SYSTEM_ERROR); \
  } while (0);
int main(int argc, char const *const *argv) {
  if (unlink(argv[0])) die("unlink failed for %s", argv[0]);
  if (unlink(argv[1])) die("unlink failed for %s", argv[1]);
  if (rmdir(argv[2]))  die("rmdir failed for %s",  argv[2]);
  argc -= 3;
  argv += 3;
  int const stdin_tty = isatty(STDIN_FILENO);
  if (!argc && stdin_tty) {
    fprintf(stderr, "TODO: print usage\n");
    return EXIT_USER_ERROR;
  }
  for_rs_parts(qni_header_sh, i) printf("%s", qni_header_sh[i]);
  printf("awk '");
  for_rs_parts(qdecompress_awk, i) printf("%s", qdecompress_awk[i]);
  printf("' <<'EOF'\n");
  for_rs_names(i) {
    int nparts = 0;
    for_rs_parts(rs[i], j) nparts = j + 1;
    printf("%d %s\n", nparts, rn[i]);
    for_rs_parts(rs[i], j) printf("%s", rs[i][j]);
  }
  printf("EOF\n");
  for_rs_parts(qni_footer_sh, i) printf("%s", qni_footer_sh[i]);
  return EXIT_NORMAL;
}
11 ni-header.sh
#!/bin/sh
# ni self-compiling source image; not intended to be edited directly
# MIT license, see https://github.com/spencertipping/ni for details
prefix=${TMPDIR:-/tmp}/ni-$USER-$$
i=0
until mkdir "$prefix-$i" 2>&1 > /dev/null; do
  i=`expr $i + 1`
done
e=$prefix-$i/ni
s=$e.c
{
2 ni-footer.sh
} > "$s"
c99 "$s" -o "$e" && exec "$e" "$s" "$prefix-$i" "$@"
5 usage
usage: ni arguments...

Arguments are either files (really quasifiles), or operators; if operators,
each one modifies the current stream in some way. Available operators:

EOF
} > "$s"
c99 "$s" -o "$e" && exec "$e" "$s" "$prefix-$i" "$@"

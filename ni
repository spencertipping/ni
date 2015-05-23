#!/bin/sh
e=`mktemp`
s=`mktemp --suffix=.c`
{
awk 'BEGIN { print "static const char *const q[];" }
{
  print $0
  gsub("\\\\", "\\\\\\\\")
  gsub("\"", "\\\"")
  q[NR] = "\"" $0 "\""
  last_q = NR
}

END {
  print "static const char *const q[] = {"
  for (i = 1; i <= last_q; i++) {
    print q[i] ","
  }
  print "(const char*) NULL};"
}' <<'EOF'
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
  for (int i = 0; q[i]; ++i) printf("%s\n", q[i]);
  return EXIT_NORMAL;
}
EOF
} > "$s"
c99 "$s" -o "$e" && exec "$e" "$s" "$@"

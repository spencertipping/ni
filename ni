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
    ra[r] = "q" gensub("\\W", "_", "g", r)
    ls = $1
    if (r) print "static const char *const " ra[r] "[] = {"
  } else {
    if (interp) {code[c++] = $0}
    gsub("\\\\", "\\\\\\\\")
    gsub("\"", "\\\"")
    print "\"" $0 "\","
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
}' <<'EOF'
24 decompress.awk
{
  if (!ls--) {
    if (r) print "(const char *const) 0};"
    interp = (rs[rn++] = r = $2) ~ /\.c$/
    ra[r] = "q" gensub("\\W", "_", "g", r)
    ls = $1
    if (r) print "static const char *const " ra[r] "[] = {"
  } else {
    if (interp) {code[c++] = $0}
    gsub("\\\\", "\\\\\\\\")
    gsub("\"", "\\\"")
    print "\"" $0 "\","
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
154 ni.c
#define for_rs_names(i)       for (int i = 0; rs[i]; ++i)
#define for_rs_parts(name, i) for (int i = 0; name[i]; ++i)
#include <sys/types.h>
#define NI_CODEC_FIXED_SIZE           1
#define NI_CODEC_USES_DELIMITERS      2
#define NI_CODEC_FORCES_EOF           4
#define NI_CODEC_USES_SEEKS           8
#define NI_CODEC_USES_BUFFERING_SEEKS 16
#define NI_CODEC_USES_ABSOLUTE_SEEKS  32
typedef struct {
  int    flags;
  size_t size;
} ni_codec;
#define NI_CODEC_ESYNTAX (-1)
#define NI_CODEC_ENREC   (-2)
#define NI_CODEC_ENFIELD (-3)
ni_codec *ni_compile_codec(char const *const s);
void ni_free_codec(ni_codec *c);
#include <sys/types.h>
#include <stdint.h>
#define NI_PACKET_OWN_BUFFER 1
#define NI_PACKET_SPLIT      2
typedef struct {
  int             flags;
  int             errno;
  ni_codec const *codec;
  char const     *data1;
  size_t          data1_size;
  char const     *data2;
  size_t          data2_size;
  char           *own_buffer;
} ni_stream_packet;
void ni_packet_unsplit(ni_stream_packet *p) {
  if (!(p->flags & NI_PACKET_SPLIT)) return;
  // TODO
}
#define NI_ERRNO_EOF (-1)
typedef struct {
  int      errno;
  mode_t   mode;
  int      fd;
  char    *buffer;
  size_t   buffer_capacity;
  size_t   buffer_size;
  off_t    read_offset;
  off_t    fill_offset;
  off_t    bytes;
  uint64_t ms;
  uint32_t delimiter_n   [256];
  float    delimiter_mean[256];
  float    delimiter_m2  [256];
  float    delimiter_m3  [256];
} ni_stream;
#define NI_READ_ERROR             (-1)
#define NI_READ_INCOMPLETE_AT_EOF (-2)
#define NI_READ_MUST_LOAD         (-3)
int ni_stream_read(ni_stream        *const s,
                   ni_codec const   *const c,
                   ni_stream_packet *const ps,
                   int               const nps,
                   int               const flags);
typedef struct ni_stream* (*cli_stream_op)(struct ni_stream *s,
                                           int               argc,
                                           char const       *argv);
typedef struct {
  char        short_form;
  char const *long_form;
  char const *args;
  char const *description;
  cli_stream_op op;
} cli_stream_option;
static cli_stream_option const cli_options[] = {
  {'a', "aggregate",  "L",  "aggregate rows by first field", },
  {'b', "buffer",     "S",  "preload data into memory"},
  {'B', "diskbuffer", "S",  "preload data through disk"},
  {'c', "count",      "",   "'uniq -c' for addressed columns"},
  {'C', "clojure",    "C*", "pipe through clojure"},
  {'D', "duplicate",  "*",  "duplicate into quasifile"},
  {'e', "encode",     "*",  "generate binary data with codec"},
  {'E', "decode",     "*",  "parse binary data with codec"},
  {'f', "fields",     "n",  "reorder, drop, create fields"},
  {'F', "fieldsplit", "S",  "split into columns on regexp"},
  {'g', "group",      "",   "group rows by addressed columns"},
  {'G', "grep",       "*",  "pipe through egrep"},
  {'H', "hadoop",     "LL", "hadoop streaming; emits hdfs qfile"},
  {'i', "into",       "?",  "write to qfile, emit qfile name"},
  {'I', "from",       "",   "read from qfiles in stream"},
  {'j', "join",       "j*", "join against qfile"},
  {'J', "java",       "C*", "pipe through java"},
  {'k', "constant",   ".",  "emit a constant value"},
  {'M', "octave",     "C*", "pipe through octave"},
  {'n', "number",     "",   "prepend line number"},
  {'o', "order",      "",   "order rows by addressed columns"},
  {'O', "rorder",     "",   "reverse order"},
  {'p', "perl",       "C*", "pipe through perl"},
  {'P', "python",     "C*", "pipe through python"},
  {'q', "sql",        ".*", "sqlite3 query with transient table"},
  {'Q', "psql",       ".*", "postgres query with transient table"},
  {'r', "ruby",       "C*", "pipe through ruby"},
  {'R', "R",          "C*", "pipe through R"},
  {'s', "sum",        "",   "running sum"},
  {'S', "delta",      "",   "delta (inverts sum)"},
  {'t', "take",       "R",  "take selected lines"},
  {'T', "tcp",        "NL", "TCP server"},
  {'u', "uniq",       "",   "'uniq' for addressed columns"},
  {'v', "vertical",   "",   "chop line into multiple lines"},
  {'V', "horizontal", "",   "join lines when addressed field is blank"},
  {'x', "canard",     "C*", "pipes through canard"},
  {'X', "shell",      "C*", "pipes through shell command"},
  {'z', "zip",        "*",  "zip columns from specified qfile"},
  {'Z', "scala",      "C*", "pipe through scala"},
  {':', "conf",       "*",  "set configuration variable"},
  {'@', "address",    "F",  "set address of next command"},
  {'^', "prepend",    "*",  "prepends qfile to stream"},
  {'[', "begin",      "",   "push new stream onto stack"},
  {']', "end",        "",   "pop stream, append to current"},
};
#define for_cli_options(i) \
  for (int i = 0; i < sizeof(cli_options) / sizeof(cli_stream_option); ++i)
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#define EXIT_NORMAL       0
#define EXIT_USER_ERROR   1
#define EXIT_SYSTEM_ERROR 2
void usage(void) {
  for_rs_parts(qusage, i) fprintf(stderr, "%s\n", qusage[i]);
  for_cli_options(i)
    fprintf(stderr, "  -%c|--%-10s  | %-2s | %s\n",
                    cli_options[i].short_form,
                    cli_options[i].long_form,
                    cli_options[i].args,
                    cli_options[i].description);
  fprintf(stderr, "\n");
}
#define die(...) \
  do { \
    fprintf(stderr, "ni: " __VA_ARGS__); \
    exit(EXIT_SYSTEM_ERROR); \
  } while (0);
int main(int const argc, char const *const *argv) {
  if (unlink(argv[0])) die("unlink failed for %s", argv[0]);
  if (unlink(argv[1])) die("unlink failed for %s", argv[1]);
  int const stdin_tty = isatty(STDIN_FILENO);
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
5 usage
usage: ni arguments...

Arguments are either files (really quasifiles), or operators; if operators,
each one modifies the current stream in some way. Available operators:

EOF
} > "$s"
c99 "$s" -o "$e" && exec "$e" "$s" "$@"

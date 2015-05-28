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
    if (interp) {code[c++] = $0}
    gsub("\\\\", "\\\\")
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
    if (interp) {code[c++] = $0}
    gsub("\\\\", "\\\\")
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
401 ni.c
#define EXIT_NORMAL       0
#define EXIT_SYSTEM_ERROR 2
#define EXIT_USER_ERROR   1
#define _ISOC99_SOURCE
#define NI_ASSERT_NOPE 2
#define NI_CODEC_FIXEDSIZE    1
#define NI_CODEC_HAS_MAX_SIZE 2
#define NI_CODEC_MAX_NESTING 256
#define NI_CODEC_SUBSIZE_CACHE 64
#define NI_LIMIT_NOPE  1
#define NI_PACKET_FIELDOFFSET_CACHE 16
#define NI_READ_DEFAULT 0
#define NI_READ_NOLOAD  1
#define NI_STREAM_QUEUE_SIZE 64
#define NI_SYSTEM_ERROR  2
#define NI_THIS_IS_A_BUG 3
#define NI_USER_ERROR    1
#define _POSIX_C_SOURCE 200112L
#define _XOPEN_SOURCE   600
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
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
typedef struct ni_codec_fieldspec {
  uint32_t offset;
  uint32_t size;
} ni_codec_fieldspec;
struct ni_codec_bytecode;
typedef struct ni_codec_bytecode ni_codec_bytecode;
typedef struct ni_codec {
  int                     flags;
  int                     n_subs;
  struct ni_codec const **subs;
} ni_codec;
struct ni_stream_packet;
ni_codec *ni_codec_compile(char const *s);
void      ni_codec_free   (ni_codec *c);
int ni_codec_fields(ni_codec                const *c,
                    struct ni_stream_packet const *p);
ni_codec_fieldspec ni_codec_field(ni_codec                const *c,
                                  struct ni_stream_packet const *p,
                                  int                            n);
off_t ni_codec_run(ni_codec_bytecode const *b,
                   char              const *data1,
                   size_t                   data1_size,
                   char              const *data2,
                   size_t                   data2_size);
typedef struct ni_stream_packet {
  ni_codec const *codec;
  char const     *data1;
  char const     *data2;
  size_t          data1_size;
  size_t          data2_size;
} ni_stream_packet;
void   ni_packet_init(ni_stream_packet       *p);
void   ni_packet_free(ni_stream_packet       *p);
size_t ni_packet_size(ni_stream_packet const *p);
int                ni_packet_fields(ni_stream_packet const *p);
ni_codec_fieldspec ni_packet_field (ni_stream_packet const *p, int f);
#define NI_ERRNO_EOF (-1)
typedef struct ni_stream {
  int      read_errno;
  int      fd;
  char    *buffer;
  int      buffer_capacity_log;
  off_t    read_offset;
  off_t    fill_offset;
  uint64_t ms;
  off_t    queue[NI_STREAM_QUEUE_SIZE];
} ni_stream;
void   ni_stream_init              (ni_stream       *s);
void   ni_stream_free              (ni_stream       *s);
bool   ni_stream_resize_buffer     (ni_stream       *s, int log);
bool   ni_stream_issplit           (ni_stream const *s);
void   ni_stream_accept            (ni_stream       *s, size_t n);
size_t ni_stream_available         (ni_stream const *s);
size_t ni_stream_remaining_capacity(ni_stream const *s);
void   ni_stream_copy_into         (char            *dest,
                                    ni_stream const *s,
                                    off_t            offset,
                                    size_t           n);
bool   ni_stream_eof (ni_stream const *s);
size_t ni_stream_fill(ni_stream *s, size_t n);
ni_stream *ni_fd_stream(int fd);
size_t ni_stream_initial_buffersize_log = 16;
size_t ni_stream_max_buffersize_log     = 20 + 6;
#define NI_READ_ERROR             (-1)
#define NI_READ_INCOMPLETE_AT_EOF (-2)
#define NI_READ_MUST_LOAD         (-3)
int ni_stream_read(ni_stream        *s,
                   ni_codec const   *c,
                   ni_stream_packet *ps,
                   int               nps,
                   int               flags);
#define for_rs_names(i)       for (int i = 0; rs[i]; ++i)
#define for_rs_parts(name, i) for (int i = 0; name[i]; ++i)
ni_codec *ni_compile_codec(char const *s)
{
}
void ni_packet_init(ni_stream_packet *const p)
{
  p->codec = 0;
  p->data1 = p->data2 = 0;
  p->data1_size = p->data2_size = 0;
}
void ni_packet_free(ni_stream_packet *const p) { free(p); }
size_t ni_packet_size(ni_stream_packet const *const p)
  { return p->data1_size + p->data2_size; }
void ni_stream_init(ni_stream *const s)
{
  s->fd = -1;
  s->read_errno = s->buffer_capacity_log
                = s->read_offset = s->fill_offset = s->ms = 0;
  s->buffer = 0;
}
void ni_stream_free(ni_stream *const s)
{
  if (s->buffer) free(s->buffer);
  free(s);
}
bool ni_stream_resize_buffer(ni_stream *const s, int const log)
{
  if (log == s->buffer_capacity_log) return true;
  size_t const new_size  = 1 << log;
  size_t const available = ni_stream_available(s);
  char *new_buffer;
  if (new_size < available)             return false;
  if (!(new_buffer = malloc(new_size))) return false;
  ni_stream_copy_into(new_buffer, s, 0, available);
  free(s->buffer);
  s->buffer              = new_buffer;
  s->buffer_capacity_log = log;
  s->read_offset         = 0;
  s->fill_offset         = available;
  return true;
}
bool ni_stream_issplit(ni_stream const *const s)
  { size_t const mask = (1 << s->buffer_capacity_log) - 1;
    return (s->fill_offset & mask) < (s->read_offset & mask); }
void ni_stream_accept(ni_stream *const s, size_t const n)
{
  ni_assert_nope(n <= ni_stream_available(s),
    "ni is trying to accept %llu bytes, more than the %llu available "
    "in the stream.\n",
    ni_ull(n),
    ni_ull(ni_stream_available(s)));
  s->read_offset = s->read_offset + n & (1 << s->buffer_capacity_log) - 1;
  if (s->read_offset == s->fill_offset) s->read_offset = s->fill_offset = 0;
}
size_t ni_stream_available(ni_stream const *const s)
  { return s->fill_offset - s->read_offset
         & (1 << s->buffer_capacity_log) - 1; }
size_t ni_stream_remaining_capacity(ni_stream const *const s)
  { return (1 << s->buffer_capacity_log) - ni_stream_available(s); }
void ni_stream_copy_into(char            *const dest,
                         ni_stream const *const s,
                         off_t            const offset,
                         size_t           const n)
{
  ni_assert_nope(offset + n <= ni_stream_available(s),
    "ni is trying to copy %llu + %llu byte(s) from a stream with only %llu"
    " byte(s) available.\n",
    ni_ull(offset),
    ni_ull(n),
    ni_ull(ni_stream_available(s)));
  if (!ni_stream_issplit(s))
    memcpy(dest, s->buffer + s->read_offset + offset, n);
  else {
    size_t const split = (1 << s->buffer_capacity_log) - s->read_offset;
    memcpy(dest,         s->buffer + s->read_offset, split);
    memcpy(dest + split, s->buffer,                  s->fill_offset);
  }
}
bool ni_stream_eof(ni_stream const *const s)
  { return s->read_errno == NI_ERRNO_EOF; }
bool ni_stream_update_errno(ni_stream *const s,
                            ssize_t    const read_size)
{
  if (read_size > 0) return false;
  s->read_errno = read_size ? errno : NI_ERRNO_EOF;
  return true;
}
size_t ni_stream_fill(ni_stream *const s, size_t const n)
{
  ni_assert_nope(s->fd >= 0,
    "ni is trying to read from a stream whose file descriptor is"
    " negative (which internally means the stream has no underlying"
    " file or device).");
  if (n > ni_stream_remaining_capacity(s)
      && !ni_stream_resize_buffer(s, ni_cintlog2(n + ni_stream_available(s))))
    return 0;
  if (ni_stream_issplit(s)) {
    ssize_t const r = read(s->fd,
                           s->buffer + s->fill_offset,
                           s->read_offset - s->fill_offset);
    if (ni_stream_update_errno(s, r)) return 0;
    s->fill_offset += r;
    return r;
  } else {
    size_t  const size = 1 << s->buffer_capacity_log;
    ssize_t const r1   = read(s->fd,
                              s->buffer + s->fill_offset,
                              size - s->fill_offset);
    if (ni_stream_update_errno(s, r1))                   return 0;
    if (s->fill_offset = s->fill_offset + r1 & size - 1) return r1;
    if (s->read_offset) {
      ssize_t const r2 = read(s->fd, s->buffer, s->read_offset);
      if (ni_stream_update_errno(s, r2)) return r1;
      s->fill_offset += r2;
      return r1 + r2;
    }
    return r1;
  }
}
ni_stream *ni_fd_stream(int const fd)
{
  ni_stream *const result = malloc(sizeof(ni_stream));
  ni_stream_init(result);
  result->fd = fd;
  return result;
}
int ni_stream_read(ni_stream        *const s,
                   ni_codec const   *const c,
                   ni_stream_packet *const ps,
                   int               const nps,
                   int               const flags)
{
  // TODO
}
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
  if (rmdir(argv[2]))  die("rmdir failed for %s",  argv[2]);
  int const stdin_tty = isatty(STDIN_FILENO);
  if (argc == 3 && stdin_tty) {
    usage();
    return EXIT_USER_ERROR;
  }
  for_rs_parts(qni_header_sh, i) printf("%s\n", qni_header_sh[i]);
  printf("awk '");
  for_rs_parts(qdecompress_awk, i) printf("%s\n", qdecompress_awk[i]);
  printf("' <<'EOF'\n");
  for_rs_names(i) {
    int nparts = 0;
    for_rs_parts(rs[i], j) nparts = j + 1;
    printf("%d %s\n", nparts, rn[i]);
    for_rs_parts(rs[i], j) printf("%s\n", rs[i][j]);
  }
  printf("EOF\n");
  for_rs_parts(qni_footer_sh, i) printf("%s\n", qni_footer_sh[i]);
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

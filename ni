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
#define NI_STATUS_REFHEAP 0
#define NI_STATUS_STREAMS 0
#define NI_STATUS_STREAMHACK_MVP 1
#define NI_STATUS_QUASIFILE 0
#define NI_STATUS_QUASIFILEHACK_MVP 1
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
void ni_image_begin(void) {}
#define NI_EXIT_NORMAL       0
#define NI_EXIT_USER_ERROR   1
#define NI_EXIT_SYSTEM_ERROR 2
#define NI_EXIT_NI_BUG       3
#if NI_STATUS_REFHEAP
typedef uint64_t ni_v;
void *ni_heap_start;
void *ni_heap_next;
void *ni_heap_limit;
#define NI_NAN_MASK  0x7ff0000000000000ull
#define NI_TYPE_MASK 0x000f000000000000ull
#define NI_DATA_MASK 0x0000ffffffffffffull
#define NI_TYPE_DBL  0  /* actual NaN value or real double */
#define NI_TYPE_PTR  1  /* pointer to something in the heap */
#define NI_TYPE_FN   2  /* pointer to native function */
#define NI_TYPE_I48  3  /* immediate 48-bit unsigned integer */
#define NI_TYPE_SM   4  /* small immediate thing */
#define ni_vdata(x) ((x) & NI_DATA_MASK)
#define ni_vptr_addr(x) (ni_heap_start             + ni_vdata(x))
#define ni_fptr_addr(x) ((void*) (&ni_image_begin) + ni_vdata(x))
int ni_vtype(const ni_v x) {
  return (x & NI_NAN_MASK) == NI_NAN_MASK
       ? NI_TYPE_DBL
       : (int) ((x & NI_TYPE_MASK) >> 48);
}
#endif
#if NI_STATUS_QUASIFILEHACK_MVP
int ni_quasifile_fd(const char *qf) {
  /* For now just support normal files. */
}
#endif
#if 0
int ni_parse_args(const int argc, const char **argv) {
  int read_fd = isatty(STDIN_FILENO) ? -1 : STDIN_FILENO;
  for (int i = 0; i < argc; ++i) {
    int fds[2];
    if (pipe(fds)) {
      fprintf(stderr, "pipe() error: %d\n", errno);
      exit(NI_EXIT_SYSTEM_ERROR);
    }
    if (argv[i][0] != '-') {
      /* We have a quasifile; get a fd for it. */
    }
  }
}
#endif
void ni_usage(void) {
  fprintf(stderr, "TODO: print usage\n");
}
#define die(...) \
  do { \
    fprintf(stderr, __VA_ARGS__); \
    exit(NI_EXIT_SYSTEM_ERROR); \
  } while (0);
int main(const int argc, const char *const *argv) {
  /* Any arguments? If not, and if stdin is a TTY, then print usage; the user
   * should be using cat instead. */
  const int stdin_tty = isatty(STDIN_FILENO);
  if (argc == 1 && stdin_tty) {
    ni_usage();
    return 1;
  }
  if (unlink(argv[0])) die("unlink failed for %s", argv[0]);
  if (unlink(argv[1])) die("unlink failed for %s", argv[1]);
  fprintf(stderr, "zomg, this works\n");
#if NI_STATUS_STREAMHACK_MVP
  /*int fd = ni_parse_args(argc, argv);*/
  return 0;
#endif
#if NI_STATUS_STREAMS
  /* At this point it's a legitimate usage. The initial stream should read
   * stdin if it's redirected; otherwise we just use the empty stream. */
  ni_stream *in = stdin_tty ? ni_empty_stream() : ni_fd_stream(STDIN_FILENO);
  /* Forward the stream to the output. If a TTY, redirect into the pager. */
  ni_stream *out = ni_fd_stream(STDOUT_FILENO);
  if (isatty(STDOUT_FILENO))
    /* Figure out which process is the pager by trial and error. If we have an
     * env-var $PAGER, use that. */
    (out = ni_process_stream(getenv("PAGER"), NULL, out))
      || (out = ni_process_stream("less", NULL, ni_fd_stream(STDOUT)))
      || (out = ni_process_stream("more", NULL, ni_fd_stream(STODUT)));
  /* TODO: parse arguments */
  return ni_connect(in, out);
#endif
}
EOF
} > "$s"
c99 "$s" -o "$e" && exec "$e" "$s" "$@"

#!/bin/sh
# ni self-compiling source image; not intended to be edited directly.
# MIT license, see https://github.com/spencertipping/ni for details
sha=${TMPDIR:-/tmp}/ni-130cc48bc97d3d865d9bc6b8459846c63d2a2bf6
[ -x "$sha" ] && exec "$sha" "$@"
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
if (r) print "0};"
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
if (r) print "0};"
print "static char const *const rn[] = {"
for (i = 0; i < rn; ++i) print "\"" rs[i] "\","
print "0};"
print "static char const *const *const rs[] = {"
for (i = 0; i < rn; ++i) print ra[rs[i]] ","
print "0};"
for (i = 0; i < c; ++i) print code[i]
}
' <<'EOF'
24 sh/decompress.awk
{
if (!ls--) {
if (r) print "0};"
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
if (r) print "0};"
print "static char const *const rn[] = {"
for (i = 0; i < rn; ++i) print "\"" rs[i] "\","
print "0};"
print "static char const *const *const rs[] = {"
for (i = 0; i < rn; ++i) print ra[rs[i]] ","
print "0};"
for (i = 0; i < c; ++i) print code[i]
}
13 sh/ni-header.sh
#!/bin/sh
# ni self-compiling source image; not intended to be edited directly.
# MIT license, see https://github.com/spencertipping/ni for details
sha=${TMPDIR:-/tmp}/ni-130cc48bc97d3d865d9bc6b8459846c63d2a2bf6
[ -x "$sha" ] && exec "$sha" "$@"
prefix=${TMPDIR:-/tmp}/ni-$USER-$$
i=0
until mkdir "$prefix-$i" 2>&1 > /dev/null; do
i=`expr $i + 1`
done
e=$prefix-$i/ni
s=$e.c
{
8 sh/ni-footer.sh
} > "$s"
if [ -n "$NI_INVISIBLE" ]; then
c99 -l m -l rt "$s" -o "$e" && rm "$s"\
&& exec "$e" "--ni:invisible" "$prefix-$i" "$@"
else
c99 -l m -l rt "$s" -o "$sha" && rm "$s" && rmdir "$prefix-$i"\
&& exec "$sha" "$@"
fi
177 ni.c
#define EXIT_NORMAL 0
#define EXIT_SYSTEM_ERROR 2
#define EXIT_USER_ERROR 1
#define _ISOC99_SOURCE
#define _POSIX_C_SOURCE 200112L
#define _XOPEN_SOURCE 600
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#define die(...)\
do {\
fprintf(stderr, "ni: " __VA_ARGS__);\
exit(EXIT_SYSTEM_ERROR);\
} while (0)
#define warn(...)\
do {\
fprintf(stderr, "ni: " __VA_ARGS__);\
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
typedef enum ni_stream_type
{
NI_STREAM_UNKNOWN,
NI_STREAM_TSV,
NI_STREAM_BINARY,
NI_STREAM_GZIP,
NI_STREAM_BZIP2,
NI_STREAM_AR,
NI_STREAM_LZO,
NI_STREAM_LZMA,
NI_STREAM_SNAPPY,
NI_STREAM_PKZIP,
NI_STREAM_TAR,
NI_STREAM_XZ,
} ni_stream_type;
struct ni_stream;
typedef struct ni_stream_ops
{
ssize_t (*read) (struct ni_stream *s, void *buf, size_t n);
ssize_t (*write)(struct ni_stream *s, void const *buf, size_t n);
void (*close)(struct ni_stream *s);
ni_stream_type (*inferred_type)(struct ni_stream const *s);
} ni_stream_ops;
#define NI_READ_EOF (-1)
#define NI_READ_EIO (-2)
#define NI_READ_ERRNO (-3)
#define NI_READ_EPROC (-4)
#define NI_WRITE_EPIPE (-1)
#define NI_WRITE_ENOSPC (-2)
#define NI_WRITE_EIO (-3)
#define NI_WRITE_ERRNO (-4)
#define NI_WRITE_EPROC (-5)
typedef struct ni_stream
{
ni_stream_ops const *ops;
int read_fd;
int write_fd;
void *opaque_state;
} ni_stream;
inline ssize_t ni_stream_read(ni_stream *const s,
void *const buf,
size_t const n)
{ return (*s->ops->read)(s, buf, n); }
inline ssize_t ni_stream_write(ni_stream *const s,
void const *const buf,
size_t const n)
{ return (*s->ops->write)(s, buf, n); }
inline void ni_stream_close(ni_stream *const s)
{ return (*s->ops->close)(s); }
inline ni_stream_type ni_stream_inferred_type(ni_stream const *const s)
{ return (*s->ops->inferred_type)(s); }
#define for_rs_names(i) for (int i = 0; rs[i]; ++i)
#define for_rs_parts(name, i) for (int i = 0; name[i]; ++i)
typedef struct ni_stream_file
{
char const *filename;
ni_stream_type inferred_type;
off_t read_offset;
off_t write_offset;
} ni_stream_file;
ssize_t ni_stream_file_read(ni_stream *const s,
void *const buf,
size_t const n)
{
return 0;
}
ssize_t ni_stream_file_write(ni_stream *const s,
void const *const buf,
size_t const n)
{
return 0;
}
void ni_stream_file_close(ni_stream *const s)
{
if (s->read_fd != -1) close(s->read_fd) || (s->read_fd = -1);
if (s->write_fd != -1) close(s->write_fd) || (s->write_fd = -1);
}
ni_stream_type ni_stream_file_type(ni_stream const *const s)
{
return ((ni_stream_file*) s->opaque_state)->inferred_type;
}
ni_stream_ops const ni_stream_file_ops = {
.read = &ni_stream_file_read,
.write = &ni_stream_file_write,
.close = &ni_stream_file_close,
.inferred_type = &ni_stream_file_type
};
ni_stream *ni_file_read(char const *const filename)
{
int const fd = open(filename, O_RDONLY | O_NONBLOCK);
if (fd == -1) return 0;
ni_stream *s = malloc(sizeof(ni_stream));
ni_stream_file *fs = malloc(sizeof(ni_stream_file));
s->ops = &ni_stream_file_ops;
s->read_fd = fd;
s->write_fd = -1;
s->opaque_state = fs;
fs->filename = filename;
fs->inferred_type = NI_STREAM_UNKNOWN;
fs->read_offset = 0;
fs->write_offset = 0;
return s;
}
int main(int argc, char const **argv) {
if (argc >= 3 && !strcmp("--ni:invisible", argv[1])) {
if (unlink(argv[0]))
warn("NI_INVISIBLE failed to unlink image %s\n", argv[0]);
else
if (rmdir(argv[2]))
warn("NI_INVISIBLE failed to unlink directory %s\n", argv[2]);
argv[2] = argv[0];
argv += 2;
argc -= 2;
}
int const stdin_tty = isatty(STDIN_FILENO);
if (argc <= 1 && stdin_tty) {
fprintf(stderr, "TODO: print usage\n");
return EXIT_USER_ERROR;
}
for_rs_parts(qsh_ni_header_sh, i) printf("%s", qsh_ni_header_sh[i]);
printf("awk '");
for_rs_parts(qsh_decompress_awk, i) printf("%s", qsh_decompress_awk[i]);
printf("' <<'EOF'\n");
for_rs_names(i) {
int nparts = 0;
for_rs_parts(rs[i], j) nparts = j + 1;
printf("%d %s\n", nparts, rn[i]);
for_rs_parts(rs[i], j) printf("%s", rs[i][j]);
}
printf("EOF\n");
for_rs_parts(qsh_ni_footer_sh, i) printf("%s", qsh_ni_footer_sh[i]);
return EXIT_NORMAL;
}
5 doc/usage
usage: ni arguments...

Arguments are either files (really quasifiles), or operators; if operators,
each one modifies the current stream in some way. Available operators:

6 perl/ni.pl
{
use v5.0;
use strict;
use warnings;
package ni;
}
17 perl/binary.pl
{
package ni::binary;
sub new {
my ($class, $in, $out) = @_;
binmode $in;
binmode $out;
bless { eof => 0,
in_fd => fileno($in),
out_fd => fileno($out),
buffer => "\0" x 4096,
offset => -1,
record => [] }, $class;
}
sub read {
my ($self) = @_;
}
}
2 ruby/ni.rb
class Ni
end
EOF
} > "$s"
if [ -n "$NI_INVISIBLE" ]; then
c99 -l m -l rt "$s" -o "$e" && rm "$s"\
&& exec "$e" "--ni:invisible" "$prefix-$i" "$@"
else
c99 -l m -l rt "$s" -o "$sha" && rm "$s" && rmdir "$prefix-$i"\
&& exec "$sha" "$@"
fi

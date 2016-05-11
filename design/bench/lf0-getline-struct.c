#include <stdio.h>
#include <string.h>

typedef struct {
  char      *data;
  long long len;
} ni_string;

int main(void) {
  char   buf[8192];
  int    gotten = 0;
  size_t bufsize = sizeof(buf);
  long long diff = 0;

  ni_string s;

  while ((gotten = getline(&buf, &bufsize, stdin)) != -1) {
    s.data = buf;
    s.len  = (long long) memchr(buf, '\t', gotten) - (long long) buf;
    diff  += s.len;
  }
  printf("%lld\n", diff);
  return 0;
}

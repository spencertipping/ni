#include <stdio.h>
#include <string.h>

int main(void) {
  char   buf[8192];
  int    gotten = 0;
  size_t bufsize = sizeof(buf);
  long long diff = 0;

  while ((gotten = getline(&buf, &bufsize, stdin)) != -1) {
    void *tab = memchr(buf, '\t', gotten);
    diff += (long long) tab - (long long) buf;
  }
  printf("%lld\n", diff);
  return 0;
}

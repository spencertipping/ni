#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

int main(void) {
  char buf[8192];
  ssize_t got;

  while (1) {
    got = read(0, buf, sizeof(buf));
    if (got >= 0) {
      printf("got %d bytes\n", got);
      fflush(stdout);
      write(1, buf, got);
    } else {
      printf("read() returned %d; errno is %d\n", got, errno);
      fflush(stdout);
    }
  }
}

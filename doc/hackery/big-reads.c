/* How much data do we get from a read() call?
 * Before running this, create a big file:
 *
 * $ dd if=/dev/zero of=bigfile bs=1M count=1024
 */

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

char buf[1 << 28];
int main() {
  int fd = open("bigfile", O_RDONLY);
  if (fd == -1) return 1;

  for (int i = 10; i <= 28; ++i) {
    lseek(fd, 0, SEEK_SET);
    printf("requested %lld bytes, got %lld\n",
           1ull << i,
           read(fd, buf, 1ull << i));
  }

  close(fd);
  return 0;
}

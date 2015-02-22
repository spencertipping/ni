#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char **argv) {
  char *buf = (char*) malloc(8192);
  char first[8192];
  size_t n = 8192;
  while (getline(&buf, &n, stdin) > 0) {
    int i = 0;
    while (buf[i] != '\t' && buf[i] != '\n') {
      first[i] = buf[i];
      i++;
    }
    first[i] = '\0';
    printf("%s\t%d\n", first, i);
  }
  return 0;
}

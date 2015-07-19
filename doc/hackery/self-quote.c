#include <stdio.h>

// NOPE NOPE NOPE
// I don't think it's possible to do what I'm trying to do because quoting and
// expansion are tied together.

#define m1 int main(int argc, char **argv) {
#define m2   for (int i = 0; strings[i]; ++i)
#define m3     printf("%d: %s\n", i, strings[i]);
#define m4   return 0;
#define m5 }

#define generate(a1, a2, a3, a4, a5) \
  const char *strings[] = {#expand(a1), #expand(a2), #expand(a3), #expand(a4), #expand(a5)}; \
  expand(a1) expand(a2) expand(a3) expand(a4) expand(a5)

generate(m1, m2, m3, m4, m5)

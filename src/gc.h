#ifndef NI_GC_H
#define NI_GC_H

#include <stdlib.h>
#include <sys/types.h>

#include "ni.h"

#define NI_HEAP_START_SIZE (1048576 * 64)

void *ni_heap;
void *ni_heap_next;
void *ni_heap_root;
void *ni_heap_limit;

void ni_heap_init(void) {
  ni_heap_next  = ni_heap = malloc(NI_HEAP_START_SIZE);
  ni_heap_limit = ni_heap + NI_HEAP_START_SIZE;
  ni_heap_root  = NULL;
}

void *ni_allocate(const size_t bytes) {
  void *result = ni_heap_next;
  ni_heap_next += bytes;

  if (ni_heap_next > ni_heap_limit) {
    fprintf(stderr, "heap overflow while allocating %ld bytes\n", bytes);
    fprintf(stderr, "(this means it's time to write a GC)\n");
    exit(NI_EXIT_NI_BUG);
  }

  return result;
}

#endif

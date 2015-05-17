#ifndef NI_OBJECT_H
#define NI_OBJECT_H

/* Low-level object system for Canard.
 * This system takes care of a few things:
 *
 * 1. Adding runtime type information to each object.
 * 2. Providing reliable memory allocation.
 * 3. Exact tracing garbage collection. */

#include <stdint.h>
#include <sys/types.h>

#include "gc.h"

/* As far as C is concerned, ni_objects don't actually exist. The pointer type
 * is just used as a marker for a downcast within type-specific methods. Never
 * dereference one of these directly. */
typedef void ni_object;

typedef struct ni_object_type ni_object_type;
typedef struct {
  /* Metadata */
  ni_object *type;   /* must be first (see below) */
  char      *name;
  size_t     size;

  /* GC functions */
  ni_object* (*allocate)(void);
} ni_object_type;


#endif

#ifndef NI_STREAM_H
#define NI_STREAM_H

#include <stdint.h>

#include "object.h"

typedef int ni_stream_mode;

typedef struct {
  ni_object_type base;

  /* Stream-specific operators */
  ni_stream_mode mode(const ni_stream*);


} ni_stream_type;

#endif

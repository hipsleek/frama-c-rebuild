/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "e_acsl_segment_tracking.h"
#ifdef E_ACSL_TEMPORAL
#  include "../../instrumentation_model/e_acsl_temporal_timestamp.h"
#endif

#include "../internals/e_acsl_timestamp_retrieval.h"

/* Local operations on temporal timestamps {{{ */
/* Remaining functionality (shared between all models) is located in e_acsl_temporal.h */
#ifdef E_ACSL_TEMPORAL
uintptr_t temporal_referent_shadow(void *addr) {
  TRY_SEGMENT(addr, return TEMPORAL_HEAP_SHADOW(addr),
              return TEMPORAL_SECONDARY_STATIC_SHADOW(addr));
  return 0;
}

uint32_t origin_timestamp(void *ptr) {
  TRY_SEGMENT_WEAK(ptr, return heap_origin_timestamp((uintptr_t)ptr),
                   return static_origin_timestamp((uintptr_t)ptr));
  return INVALID_TEMPORAL_TIMESTAMP;
}

uint32_t referent_timestamp(void *ptr) {
  TRY_SEGMENT(ptr, return heap_referent_timestamp((uintptr_t)ptr),
              return static_referent_timestamp((uintptr_t)ptr));
  return INVALID_TEMPORAL_TIMESTAMP;
}

void store_temporal_referent(void *ptr, uint32_t ref) {
  TRY_SEGMENT(ptr, heap_store_temporal_referent((uintptr_t)ptr, ref),
              static_store_temporal_referent((uintptr_t)ptr, ref));
}
#endif
/* }}} */

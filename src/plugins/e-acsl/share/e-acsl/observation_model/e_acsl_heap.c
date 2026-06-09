/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "internals/e_acsl_heap_tracking.h"

#include "e_acsl_heap.h"

size_t eacsl_get_heap_allocation_size(void) {
  return get_heap_internal_allocation_size();
}

size_t eacsl_get_heap_allocated_blocks(void) {
  return get_heap_internal_allocated_blocks();
}

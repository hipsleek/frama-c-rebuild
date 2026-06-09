/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "../../internals/e_acsl_bits.h"
#include "../../internals/e_acsl_rtl_io.h"
#include "e_acsl_segment_tracking.h"
#include "e_acsl_shadow_layout.h"

#include "../internals/e_acsl_omodel_debug.h"

#define E_ACSL_MMODEL_DESC "shadow memory"

void describe_observation_model() {
  rtl_printf(" * Memory tracking: %s\n", E_ACSL_MMODEL_DESC);
  rtl_printf(" *           Heap %d MB\n", E_ACSL_HEAP_SIZE);
  rtl_printf(" *          Stack %d MB\n", E_ACSL_STACK_SIZE);
  rtl_printf(" *            TLS %d MB\n", E_ACSL_TLS_SIZE);
  rtl_printf(" *   Thread stack %d MB\n", E_ACSL_THREAD_STACK_SIZE);
}

int allocated(uintptr_t addr, long size, uintptr_t base) {
  if (get_safe_location(addr, size) != NULL)
    return 1;

  TRY_SEGMENT_WEAK(addr, return heap_allocated(addr, size, base),
                   return static_allocated(addr, size, base));
  if (!IS_ON_VALID(addr))
    return 0;
  return 0;
}

int readonly(void *ptr) {
  uintptr_t addr = (uintptr_t)ptr;

  memory_location *safeloc = get_safe_location(addr, 1);
  if (safeloc != NULL)
    return !safeloc->writeable;

  return IS_ON_GLOBAL(addr) && global_readonly(addr) ? 1 : 0;
}

int writeable(uintptr_t addr, long size, uintptr_t base_ptr) {
  memory_location *safeloc = get_safe_location(addr, 1);
  if (safeloc != NULL)
    return !safeloc->writeable;

  return allocated(addr, size, base_ptr) && !readonly((void *)addr);
}

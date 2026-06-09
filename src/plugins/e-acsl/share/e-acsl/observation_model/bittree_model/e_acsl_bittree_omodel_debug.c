/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "../../internals/e_acsl_private_assert.h"
#include "e_acsl_bittree.h"

#include "../internals/e_acsl_omodel_debug.h"

#define E_ACSL_MMODEL_DESC "patricia trie"

void describe_observation_model() {
  rtl_printf(" * Memory tracking: %s\n", E_ACSL_MMODEL_DESC);
}

/** \brief same as ::lookup_allocated but return either `1` or `0` depending
    on whether the memory block described by this function's arguments is
    allocated or not.
    NOTE: Should have same signature in all models. */
int allocated(uintptr_t addr, long size, uintptr_t base) {
  if (get_safe_location(addr, size) != NULL)
    return 1;

  return lookup_allocated((void *)addr, size, (void *)base) == NULL ? 0 : 1;
}

int readonly(void *ptr) {
  memory_location *safeloc = get_safe_location((uintptr_t)ptr, 1);
  if (safeloc != NULL)
    return !safeloc->writeable;

  bt_block *blk = bt_find(ptr);
  private_assert(blk != NULL, "Readonly on unallocated memory\n", NULL);
  return blk->is_readonly;
}

int writeable(uintptr_t addr, long size, uintptr_t base_ptr) {
  memory_location *safeloc = get_safe_location(addr, size);
  if (safeloc != NULL)
    return safeloc->writeable;

  return allocated(addr, size, base_ptr) && !readonly((void *)addr);
}

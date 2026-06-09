/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "e_acsl_malloc.h"

#define eacsl_create_mspace export_alias(create_mspace)

extern mspace eacsl_create_mspace(size_t, int);

struct memory_spaces mem_spaces = {
    .rtl_mspace = NULL,
    .heap_mspace = NULL,
    .rtl_start = 0,
    .rtl_end = 0,
    .heap_start = 0,
    .heap_end = 0,
    .heap_mspace_least = 0,
};

mspace eacsl_create_locked_mspace(size_t size) {
  return eacsl_create_mspace(size, 1);
}

/* \brief Create two memory spaces, one for RTL and the other for application
   memory. This function *SHOULD* be called before any allocations are made
   otherwise execution fails */
void eacsl_make_memory_spaces(size_t rtl_size, size_t heap_size) {
  mem_spaces.rtl_mspace = eacsl_create_locked_mspace(rtl_size);
  mem_spaces.heap_mspace = eacsl_create_locked_mspace(heap_size);
  /* Do not use `eacsl_mspace_least_addr` here, as it returns the address of the
     mspace header. */
  mem_spaces.rtl_start =
      (uintptr_t)eacsl_mspace_malloc(mem_spaces.rtl_mspace, 1);
  mem_spaces.rtl_end = mem_spaces.rtl_start + rtl_size;
  mem_spaces.heap_start =
      (uintptr_t)eacsl_mspace_malloc(mem_spaces.heap_mspace, 1);
  mem_spaces.heap_end = mem_spaces.heap_start + heap_size;
  /* Save initial least address of heap memspace. This address is used later
     to check whether memspace has been moved. */
  mem_spaces.heap_mspace_least =
      (uintptr_t)eacsl_mspace_least_addr(mem_spaces.heap_mspace);
}

void eacsl_destroy_memory_spaces() {
  eacsl_destroy_mspace(mem_spaces.rtl_mspace);
  eacsl_destroy_mspace(mem_spaces.heap_mspace);
}

int is_pow_of_2(size_t x) {
  while (((x & 1) == 0) && x > 1) /* while x is even and > 1 */
    x >>= 1;
  return (x == 1);
}

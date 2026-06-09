/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/*! ***********************************************************************
 * \file
 * \brief  User API to query E-ACSL about the state of heap allocation.
 **************************************************************************/

#ifndef E_ACSL_HEAP
#define E_ACSL_HEAP

#include <stddef.h>

#include "../internals/e_acsl_alias.h"

#define eacsl_heap_allocation_size      export_alias(heap_allocation_size)
#define eacsl_heap_allocated_blocks     export_alias(heap_allocated_blocks)
#define eacsl_get_heap_allocation_size  export_alias(get_heap_allocation_size)
#define eacsl_get_heap_allocated_blocks export_alias(get_heap_allocated_blocks)

/*! \brief A variable holding the number of bytes in heap application allocation. */
extern size_t eacsl_heap_allocation_size;
/*! \brief A variable holding the number of blocks in heap application allocation. */
extern size_t eacsl_heap_allocated_blocks;

/*! Return the number of bytes in heap application allocation. */
size_t eacsl_get_heap_allocation_size(void) __attribute__((FC_BUILTIN));

/*! Return the number of blocks in heap application allocation. */
size_t eacsl_get_heap_allocated_blocks(void) __attribute__((FC_BUILTIN));

#endif // E_ACSL_HEAP

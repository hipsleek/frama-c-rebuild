/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/*! ***********************************************************************
 * \file
 * \brief Functionality to report/track memory leaks. Shared between models
 **************************************************************************/

#ifndef E_ACSL_HEAP_TRACKING
#define E_ACSL_HEAP_TRACKING

#include <stddef.h>

/* Return the number of bytes in heap application allocation */
size_t get_heap_internal_allocation_size(void);

/* Return the number of blocks in heap application allocation */
size_t get_heap_internal_allocated_blocks(void);

/* Update heap allocation stats */
void update_heap_allocation(long size);

/* If E_ACSL_VERBOSE or E_ACSL_DEBUG are set, print a message if there is still
 * some allocated memory on the heap. */
void report_heap_leacks();

#endif // E_ACSL_HEAP_TRACKING

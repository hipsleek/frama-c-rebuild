/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_ALLOC_AXIOMATIC_H
#define __FC_ALLOC_AXIOMATIC_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"
#include "__fc_define_wchar_t.h"

__BEGIN_DECLS

/*@ ghost __FC_EXTERN int __fc_heap_status; */

/*@ axiomatic dynamic_allocation {
  @   predicate is_allocable{L}(integer n) // Can a block of n bytes be allocated?
  @     reads __fc_heap_status;
  @   // The logic label L is not used, but it must be present because the
  @   // predicate depends on the memory state
  @   axiom never_allocable{L}:
  @     \forall integer i;
  @        i < 0 || i > __FC_SIZE_MAX ==> !is_allocable(i);
  @ }
*/

__END_DECLS

__POP_FC_STDLIB
#endif

/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_ALLOCA_H
#define __FC_ALLOCA_H
#include "features.h"
__PUSH_FC_STDLIB
#include <stdlib.h>

__BEGIN_DECLS

/*@ ghost __FC_EXTERN int __fc_stack_status; */

// Note: alloca is considered to never fail, unlike malloc
// Currently, ACSL does not allow specifying that the memory allocated by
// alloca must be freed at the end of the execution of its caller,
// therefore this responsibility is given to the user of this function.
/*@
  allocates \result;
  assigns __fc_stack_status \from size, __fc_stack_status;
  assigns \result \from indirect:size, indirect:__fc_stack_status;
  ensures allocation: \fresh(\result,size);
*/
extern void *alloca(size_t size);

__END_DECLS
__POP_FC_STDLIB
#endif

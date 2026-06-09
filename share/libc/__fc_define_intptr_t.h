/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_INTPTR_T_H
#define __FC_DEFINE_INTPTR_T_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"

__BEGIN_DECLS

#ifdef __INTPTR_T
#ifndef __intptr_t_defined
typedef __INTPTR_T intptr_t;
#define INTPTR_MIN __FC_INTPTR_MIN
#define INTPTR_MAX __FC_INTPTR_MAX
#define __intptr_t_defined 1
#endif
#endif

__END_DECLS

__POP_FC_STDLIB
#endif

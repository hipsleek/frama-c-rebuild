/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_MBSTATE_T_H
#define __FC_DEFINE_MBSTATE_T_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"
__BEGIN_DECLS
#ifndef __mbstate_t_defined
typedef struct __fc_mbstate_t { int __count; char __value[4]; } mbstate_t;
#define __mbstate_t_defined 1
#endif
__END_DECLS
__POP_FC_STDLIB
#endif

/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_TIME_T_H
#define __FC_DEFINE_TIME_T_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"
__BEGIN_DECLS
#ifndef __time_t_defined
typedef __FC_TIME_T time_t;
#define __time_t_defined 1
#endif
__END_DECLS
__POP_FC_STDLIB
#endif

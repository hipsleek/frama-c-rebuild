/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_SUSECONDS_T_H
#define __FC_DEFINE_SUSECONDS_T_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#ifndef __suseconds_t_defined
typedef signed int suseconds_t;
#define __suseconds_t_defined 1
#endif
__END_DECLS
__POP_FC_STDLIB
#endif

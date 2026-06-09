/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_SIGSET_T_H
#define __FC_DEFINE_SIGSET_T_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#ifndef __sigset_t_defined
typedef unsigned long sigset_t;
#define __sigset_t_defined 1
#endif
__END_DECLS
__POP_FC_STDLIB
#endif

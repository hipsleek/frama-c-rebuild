/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_OFF_T_H
#define __FC_DEFINE_OFF_T_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"
__BEGIN_DECLS
#ifndef __off_t_defined
typedef long int off_t;
#define __off_t_defined 1
#endif
#ifndef __off64_t_defined
typedef __INT64_T off64_t;
#define __off64_t_defined 1
#endif
__END_DECLS
__POP_FC_STDLIB
#endif


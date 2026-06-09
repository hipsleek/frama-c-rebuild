/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_FS_CNT_H
#define __FC_DEFINE_FS_CNT_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#ifndef __fsblkcnt_t_defined
typedef unsigned long fsblkcnt_t;
#define __fsblkcnt_t_defined 1
#endif
#ifndef __fsfilcnt_t_defined
typedef unsigned long fsfilcnt_t;
#define __fsfilcnt_t_defined 1
#endif
__END_DECLS
__POP_FC_STDLIB
#endif

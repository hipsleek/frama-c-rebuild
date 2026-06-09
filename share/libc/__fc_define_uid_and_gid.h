/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_UID_AND_GID_H
#define __FC_DEFINE_UID_AND_GID_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#ifndef __gid_t_defined
typedef unsigned int gid_t;
#define __gid_t_defined 1
#endif
#ifndef __uid_t_defined
typedef unsigned int uid_t;
#define __uid_t_defined 1
#endif
__END_DECLS
__POP_FC_STDLIB
#endif


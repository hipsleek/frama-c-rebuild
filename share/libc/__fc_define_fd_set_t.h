/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_FD_SET_T_H
#define __FC_DEFINE_FD_SET_T_H
#include "features.h"
__PUSH_FC_STDLIB
#define FD_SETSIZE 1024
#define NFDBITS (8 * sizeof(long))
__BEGIN_DECLS
typedef struct __fc_fd_set { long __fc_fd_set[FD_SETSIZE / NFDBITS]; } fd_set;

__END_DECLS
__POP_FC_STDLIB
#endif

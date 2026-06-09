/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_ULIMIT_H
#define __FC_ULIMIT_H
#include "features.h"
__PUSH_FC_STDLIB
#include <errno.h>

__BEGIN_DECLS

#define UL_GETFSIZE 1
#define UL_SETFSIZE 2

/*@
  assigns \result, errno \from indirect:cmd;
  //missing: from 'current process'
*/
extern long ulimit(int cmd, ...);

__END_DECLS

__POP_FC_STDLIB
#endif

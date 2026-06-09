/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_PARAM_H
#define __FC_SYS_PARAM_H
#include "../features.h"
__PUSH_FC_STDLIB

// Note: sys/param.h is not a POSIX file. This is provided as a best-effort
// basis to support projects using constants such as PATH_MAX, which should
// be defined in "limits.h" according to POSIX. For instance, in Linux,
// PATH_MAX is defined in the non-POSIX file linux/limits.h.

#include <limits.h>

#define MAX(x,y) ((x)>=(y)?(x):(y))
#define MIN(x,y) ((x)<=(y)?(x):(y))

__POP_FC_STDLIB
#endif

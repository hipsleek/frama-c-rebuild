/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_SSIZE_T_H
#define __FC_DEFINE_SSIZE_T_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"
__BEGIN_DECLS
// This file may be included by non-POSIX machdeps (e.g. via sys/types.h),
// so we must check if ssize_t should be defined
#ifdef _POSIX_C_SOURCE
typedef __SSIZE_T ssize_t;
#define SSIZE_MAX __SSIZE_MAX
#endif
__END_DECLS
__POP_FC_STDLIB
#endif

/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/* ISO C: 7.15 */
#ifndef __FC_STDARG_H
#define __FC_STDARG_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h" // for __FC_VA_LIST_T
__BEGIN_DECLS
typedef __FC_VA_LIST_T va_list;
__END_DECLS
#define va_arg(a,b) __builtin_va_arg(a,b)
#define va_copy(a,b) __builtin_va_copy(a,b)
#define va_end(a) __builtin_va_end(a)
#define va_start(a,b) __builtin_va_start(a,b)
__POP_FC_STDLIB
#endif

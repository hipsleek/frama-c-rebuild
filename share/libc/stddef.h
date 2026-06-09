/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_STDDEF_H
#define __FC_STDDEF_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"
__BEGIN_DECLS
#ifndef __ptrdiff_t_defined
typedef __PTRDIFF_T ptrdiff_t;
#define __ptrdiff_t_defined 1
#endif

// max_align_t is not defined in every machdeps
#ifdef __MAX_ALIGN_T
#ifndef __max_align_t_defined
typedef __MAX_ALIGN_T max_align_t;
#define __max_align_t_defined 1
#endif
#endif
__END_DECLS
#include "__fc_define_size_t.h"
#include "__fc_define_ssize_t.h"
#include "__fc_define_wchar_t.h"
#include "__fc_define_null.h"
#define offsetof(type, member) __builtin_offsetof(type,member)

__POP_FC_STDLIB
#endif

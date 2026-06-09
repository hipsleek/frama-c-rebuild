/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_WCHAR_T_H
#define __FC_DEFINE_WCHAR_T_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#include "__fc_machdep.h"
#if !defined(__cplusplus)
/* wchar_t is a keyword in C++ and shall not be a typedef. */
typedef __WCHAR_T wchar_t;
#else
typedef __WCHAR_T fc_wchar_t;
#endif
__END_DECLS
__POP_FC_STDLIB
#endif

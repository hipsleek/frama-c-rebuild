/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_STDALIGN_H
#define __FC_STDALIGN_H

#if __STDC_VERSION__ > 201710L
/* The header is empty after C17 */
#else

#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#define alignas _Alignas
#define alignof _Alignof

#define __alignas_is_defined 1
#define __alignof_is_defined 1

__END_DECLS
__POP_FC_STDLIB
#endif

#endif

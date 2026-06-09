/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_STDBOOL_H
#define __FC_STDBOOL_H

// Still defined in C23
#define __bool_true_false_are_defined 1

#if __STDC_VERSION__ > 201710L
/* bool, true and false are keywords after C17 */
#else

// In C++, bool, true and false are native values
#ifndef __cplusplus

#define bool _Bool
#define true 1
#define false 0

#endif

#endif

#endif

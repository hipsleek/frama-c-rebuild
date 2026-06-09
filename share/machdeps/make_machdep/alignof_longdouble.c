/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"

_Static_assert(ALIGNOF(long double) != 1, "alignof_longdouble is 1");
_Static_assert(ALIGNOF(long double) != 2, "alignof_longdouble is 2");
_Static_assert(ALIGNOF(long double) != 4, "alignof_longdouble is 4");
_Static_assert(ALIGNOF(long double) != 8, "alignof_longdouble is 8");
_Static_assert(ALIGNOF(long double) != 16, "alignof_longdouble is 16");
_Static_assert(ALIGNOF(long double) != 32, "alignof_longdouble is 32");

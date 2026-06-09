/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
_Static_assert(ALIGNOF(long long) != 1, "alignof_longlong is 1");
_Static_assert(ALIGNOF(long long) != 2, "alignof_longlong is 2");
_Static_assert(ALIGNOF(long long) != 4, "alignof_longlong is 4");
_Static_assert(ALIGNOF(long long) != 8, "alignof_longlong is 8");
_Static_assert(ALIGNOF(long long) != 16, "alignof_longlong is 16");
_Static_assert(ALIGNOF(long long) != 32, "alignof_longlong is 32");

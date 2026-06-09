/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
_Static_assert(ALIGNOF(long) != 1, "alignof_long is 1");
_Static_assert(ALIGNOF(long) != 2, "alignof_long is 2");
_Static_assert(ALIGNOF(long) != 4, "alignof_long is 4");
_Static_assert(ALIGNOF(long) != 8, "alignof_long is 8");
_Static_assert(ALIGNOF(long) != 16, "alignof_long is 16");
_Static_assert(ALIGNOF(long) != 32, "alignof_long is 32");

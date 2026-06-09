/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
_Static_assert(ALIGNOF(int) != 1, "alignof_int is 1");
_Static_assert(ALIGNOF(int) != 2, "alignof_int is 2");
_Static_assert(ALIGNOF(int) != 4, "alignof_int is 4");
_Static_assert(ALIGNOF(int) != 8, "alignof_int is 8");
_Static_assert(ALIGNOF(int) != 16, "alignof_int is 16");
_Static_assert(ALIGNOF(int) != 32, "alignof_int is 32");

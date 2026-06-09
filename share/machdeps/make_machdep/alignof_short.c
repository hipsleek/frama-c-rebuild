/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
_Static_assert(ALIGNOF(short) != 1, "alignof_short is 1");
_Static_assert(ALIGNOF(short) != 2, "alignof_short is 2");
_Static_assert(ALIGNOF(short) != 4, "alignof_short is 4");
_Static_assert(ALIGNOF(short) != 8, "alignof_short is 8");
_Static_assert(ALIGNOF(short) != 16, "alignof_short is 16");
_Static_assert(ALIGNOF(short) != 32, "alignof_short is 32");

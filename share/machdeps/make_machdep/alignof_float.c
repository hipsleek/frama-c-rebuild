/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"

_Static_assert(ALIGNOF(float) != 1, "alignof_float is 1");
_Static_assert(ALIGNOF(float) != 2, "alignof_float is 2");
_Static_assert(ALIGNOF(float) != 4, "alignof_float is 4");
_Static_assert(ALIGNOF(float) != 8, "alignof_float is 8");
_Static_assert(ALIGNOF(float) != 16, "alignof_float is 16");
_Static_assert(ALIGNOF(float) != 32, "alignof_float is 32");

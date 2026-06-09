/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"

typedef char array[1] __attribute__((__aligned__));

_Static_assert(ALIGNOF(array)!=1, "alignof_aligned is 1");
_Static_assert(ALIGNOF(array)!=2, "alignof_aligned is 2");
_Static_assert(ALIGNOF(array)!=4, "alignof_aligned is 4");
_Static_assert(ALIGNOF(array)!=8, "alignof_aligned is 8");
_Static_assert(ALIGNOF(array)!=16, "alignof_aligned is 16");
_Static_assert(ALIGNOF(array)!=32, "alignof_aligned is 32");

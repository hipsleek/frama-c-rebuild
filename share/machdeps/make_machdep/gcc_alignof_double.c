/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"

_Static_assert(__alignof__(double) != 1, "gcc_alignof_double is 1");
_Static_assert(__alignof__(double) != 2, "gcc_alignof_double is 2");
_Static_assert(__alignof__(double) != 4, "gcc_alignof_double is 4");
_Static_assert(__alignof__(double) != 8, "gcc_alignof_double is 8");
_Static_assert(__alignof__(double) != 16, "gcc_alignof_double is 16");
_Static_assert(__alignof__(double) != 32, "gcc_alignof_double is 32");

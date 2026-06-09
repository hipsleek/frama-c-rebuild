/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"

_Static_assert(__alignof__(float) != 1, "gcc_alignof_float is 1");
_Static_assert(__alignof__(float) != 2, "gcc_alignof_float is 2");
_Static_assert(__alignof__(float) != 4, "gcc_alignof_float is 4");
_Static_assert(__alignof__(float) != 8, "gcc_alignof_float is 8");
_Static_assert(__alignof__(float) != 16, "gcc_alignof_float is 16");
_Static_assert(__alignof__(float) != 32, "gcc_alignof_float is 32");

/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"

_Static_assert(__alignof__(void *) != 1, "gcc_alignof_ptr is 1");
_Static_assert(__alignof__(void *) != 2, "gcc_alignof_ptr is 2");
_Static_assert(__alignof__(void *) != 4, "gcc_alignof_ptr is 4");
_Static_assert(__alignof__(void *) != 8, "gcc_alignof_ptr is 8");
_Static_assert(__alignof__(void *) != 16, "gcc_alignof_ptr is 16");
_Static_assert(__alignof__(void *) != 32, "gcc_alignof_ptr is 32");

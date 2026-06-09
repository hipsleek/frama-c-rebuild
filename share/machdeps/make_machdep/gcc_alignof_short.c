/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
_Static_assert(__alignof__(short) != 1, "gcc_alignof_short is 1");
_Static_assert(__alignof__(short) != 2, "gcc_alignof_short is 2");
_Static_assert(__alignof__(short) != 4, "gcc_alignof_short is 4");
_Static_assert(__alignof__(short) != 8, "gcc_alignof_short is 8");
_Static_assert(__alignof__(short) != 16, "gcc_alignof_short is 16");
_Static_assert(__alignof__(short) != 32, "gcc_alignof_short is 32");

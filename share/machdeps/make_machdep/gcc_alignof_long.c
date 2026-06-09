/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
_Static_assert(__alignof__(long) != 1, "gcc_alignof_long is 1");
_Static_assert(__alignof__(long) != 2, "gcc_alignof_long is 2");
_Static_assert(__alignof__(long) != 4, "gcc_alignof_long is 4");
_Static_assert(__alignof__(long) != 8, "gcc_alignof_long is 8");
_Static_assert(__alignof__(long) != 16, "gcc_alignof_long is 16");
_Static_assert(__alignof__(long) != 32, "gcc_alignof_long is 32");

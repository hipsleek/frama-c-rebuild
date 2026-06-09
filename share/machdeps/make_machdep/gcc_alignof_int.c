/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
_Static_assert(__alignof__(int) != 1, "gcc_alignof_int is 1");
_Static_assert(__alignof__(int) != 2, "gcc_alignof_int is 2");
_Static_assert(__alignof__(int) != 4, "gcc_alignof_int is 4");
_Static_assert(__alignof__(int) != 8, "gcc_alignof_int is 8");
_Static_assert(__alignof__(int) != 16, "gcc_alignof_int is 16");
_Static_assert(__alignof__(int) != 32, "gcc_alignof_int is 32");

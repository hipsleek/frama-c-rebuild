/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"

_Static_assert(__alignof__(long double) != 1, "gcc_alignof_longdouble is 1");
_Static_assert(__alignof__(long double) != 2, "gcc_alignof_longdouble is 2");
_Static_assert(__alignof__(long double) != 4, "gcc_alignof_longdouble is 4");
_Static_assert(__alignof__(long double) != 8, "gcc_alignof_longdouble is 8");
_Static_assert(__alignof__(long double) != 16, "gcc_alignof_longdouble is 16");
_Static_assert(__alignof__(long double) != 32, "gcc_alignof_longdouble is 32");

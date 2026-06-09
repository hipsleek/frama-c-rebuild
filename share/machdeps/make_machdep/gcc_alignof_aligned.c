/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"

typedef char array[1] __attribute__((__aligned__));

_Static_assert(__alignof__(array)!=1, "gcc_alignof_aligned is 1");
_Static_assert(__alignof__(array)!=2, "gcc_alignof_aligned is 2");
_Static_assert(__alignof__(array)!=4, "gcc_alignof_aligned is 4");
_Static_assert(__alignof__(array)!=8, "gcc_alignof_aligned is 8");
_Static_assert(__alignof__(array)!=16, "gcc_alignof_aligned is 16");
_Static_assert(__alignof__(array)!=32, "gcc_alignof_aligned is 32");

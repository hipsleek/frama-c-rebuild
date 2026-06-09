/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
#include <stddef.h>

_Static_assert(__alignof__(max_align_t) != 1, "gcc_alignof_max_align_t is 1");
_Static_assert(__alignof__(max_align_t) != 2, "gcc_alignof_max_align_t is 2");
_Static_assert(__alignof__(max_align_t) != 4, "gcc_alignof_max_align_t is 4");
_Static_assert(__alignof__(max_align_t) != 8, "gcc_alignof_max_align_t is 8");
_Static_assert(__alignof__(max_align_t) != 16, "gcc_alignof_max_align_t is 16");
_Static_assert(__alignof__(max_align_t) != 32, "gcc_alignof_max_align_t is 32");

/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"

_Static_assert(ALIGNOF(void()) != 1, "alignof_fun is 1");
_Static_assert(ALIGNOF(void()) != 2, "alignof_fun is 2");
_Static_assert(ALIGNOF(void()) != 4, "alignof_fun is 4");
_Static_assert(ALIGNOF(void()) != 8, "alignof_fun is 8");
_Static_assert(ALIGNOF(void()) != 16, "alignof_fun is 16");
_Static_assert(ALIGNOF(void()) != 32, "alignof_fun is 32");

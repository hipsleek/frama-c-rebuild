/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include <limits.h>
#include <stdlib.h>

#if defined(RAND_MAX)
int rand_max_is = RAND_MAX;
#endif

/* NB: MB_LEN_MAX is the maximal value of MB_CUR_MAX;
   however, the current Frama-C libc is not equipped to
   fully deal with a non-constant MB_CUR_MAX
*/
#if defined(MB_LEN_MAX)
size_t mb_cur_max_is = ((size_t)MB_LEN_MAX);
#endif

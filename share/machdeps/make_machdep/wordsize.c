/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include <features.h>

#if defined __has_include
#  if __has_include (<bits/reg.h>) // musl defines __WORDSIZE here
#    include <bits/reg.h>
#  endif
#endif

#if defined(__WORDSIZE)
const int wordsize_is = __WORDSIZE;
#endif

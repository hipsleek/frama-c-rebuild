/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "inttypes.h"
__PUSH_FC_STDLIB

intmax_t imaxabs(intmax_t c) {
  if (c>0) return c; 
  else return (-c);
}

imaxdiv_t imaxdiv(intmax_t numer, intmax_t denom){
  imaxdiv_t r;
  r.quot=numer/denom;
  r.rem=numer%denom;
  return r;
}

__POP_FC_STDLIB

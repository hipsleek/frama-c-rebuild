/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/* ISO C: 7.12 */

#include "math.h"
__PUSH_FC_STDLIB

double fabs(double x){
  if(x==0.0) return 0.0;
  if (x>0.0) return x;
  return -x;
}

float fabsf(float x)
{
  if (x == 0.0f) {
    return 0.0f;
  } else if (x > 0.0f) {
    return x;
  } else {
    return -x;
  }
}

int __finitef(float f)
{
  union __fc_u_finitef { float f ; unsigned short w[2] ; } u ;
  unsigned short usExp ;

  u.f = f ;            /* Initialize for word access */
  usExp = (u.w[1] & 0x7F80) ;   /* Isolate the exponent */
  usExp >>= 7 ;                 /* Right align */

  /* A floating point value is invalid, if the exponent is 0xff */
  return !(usExp == 0xff) ;
}

int __finite(double d)
{
  union __fc_u_finite { double d ; unsigned short w[4] ; } u ;
  unsigned short usExp ;

  u.d = d ;            /* Initialize for word access */
  usExp = (u.w[3] & 0x7F80) ;   /* Isolate the exponent */
  usExp >>= 7 ;                 /* Right align */

  /* A floating point value is invalid, if the exponent is 0xff */
  return !(usExp == 0xff) ;
}

__POP_FC_STDLIB

/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_TIMESPEC_H
#define __FC_DEFINE_TIMESPEC_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
struct timespec {
  long    tv_sec;
  long    tv_nsec;
};
__END_DECLS
__POP_FC_STDLIB
#endif

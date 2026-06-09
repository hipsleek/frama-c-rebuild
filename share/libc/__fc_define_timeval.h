/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_TIMEVAL_H
#define __FC_DEFINE_TIMEVAL_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#include "__fc_define_suseconds_t.h"
#include "__fc_define_time_t.h"
struct timeval {
  time_t         tv_sec;
  suseconds_t    tv_usec;
};
__END_DECLS
__POP_FC_STDLIB
#endif

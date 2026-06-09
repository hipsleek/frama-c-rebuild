/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "time.h"
#include "__fc_builtin.h"
__PUSH_FC_STDLIB

char __fc_ctime[26];
struct tm __fc_time_tm;
struct tm __fc_getdate;

extern char *ctime(const time_t *timer) {
  //@ assert \valid_read(timer);
  //@ assert \initialized(timer);
  Frama_C_make_unknown(__fc_ctime, 26);
  __fc_ctime[25] = 0;
  return __fc_ctime;
}

__POP_FC_STDLIB

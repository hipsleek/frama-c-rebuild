/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_TIMES_H
#define __FC_SYS_TIMES_H

#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#include <time.h>

struct tms
{
  clock_t tms_utime;
  clock_t tms_stime;
  clock_t tms_cutime;
  clock_t tms_cstime;
};

/*@ requires valid_buffer: \valid(buffer);
    assigns \result, *buffer \from __fc_time; */
extern clock_t times (struct tms *buffer);

__END_DECLS
__POP_FC_STDLIB
#endif

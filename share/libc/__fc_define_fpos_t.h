/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_FPOS_T_H
#define __FC_DEFINE_FPOS_T_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

struct __fc_pos_t { unsigned long __fc_stdio_position; };
typedef struct __fc_pos_t fpos_t;

__END_DECLS

__POP_FC_STDLIB
#endif

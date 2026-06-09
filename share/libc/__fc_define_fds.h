/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_FDS_H
#define __FC_DEFINE_FDS_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "__fc_define_max_open_files.h"

// __fc_fds represents the state of open file descriptors.
__FC_EXTERN volatile int __fc_fds[__FC_MAX_OPEN_FILES];

__END_DECLS

__POP_FC_STDLIB
#endif

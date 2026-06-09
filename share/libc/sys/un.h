/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_UN_H
#define __FC_SYS_UN_H
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#include "../__fc_define_sa_family_t.h"

struct sockaddr_un
  {
    sa_family_t sun_family;
    // Note: the length has been hard-coded to the value typically found in
    // Linux. Move it to the machdep to support other implementations.
    char sun_path[108];         /* Path name.  */
  };

__END_DECLS
__POP_FC_STDLIB
#endif

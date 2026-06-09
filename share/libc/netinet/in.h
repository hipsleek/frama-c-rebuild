/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

// According to POSIX, definitions from both arpa/inet.h and netinet/in.h
// may be exported by one another, so everything is defined in a common file.

#ifndef __FC_NETINET_IN_H
#define __FC_NETINET_IN_H
#include "../features.h"
__PUSH_FC_STDLIB
#include "../__fc_inet.h"
__POP_FC_STDLIB
#endif

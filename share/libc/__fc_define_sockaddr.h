/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_SOCKADDR_H
#define __FC_DEFINE_SOCKADDR_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_sa_family_t.h"
__BEGIN_DECLS
struct sockaddr {
  sa_family_t		sa_family;	/* address family, AF_xxx	*/
  char			sa_data[14];	/* 14 bytes of protocol address	*/
};
__END_DECLS
__POP_FC_STDLIB
#endif


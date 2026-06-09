/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_IOVEC_H
#define __FC_DEFINE_IOVEC_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_size_t.h"
__BEGIN_DECLS
struct iovec {
  void   *iov_base;
  size_t  iov_len;
};
__END_DECLS
__POP_FC_STDLIB
#endif


/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_FILE_H
#define __FC_DEFINE_FILE_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_stat.h"
#include "__fc_define_fpos_t.h"

__BEGIN_DECLS

#ifndef __FILE_defined
struct __fc_FILE {
  unsigned int __fc_FILE_id;
  unsigned int __fc_FILE_data;
};
typedef struct __fc_FILE FILE;
#define __FILE_defined 1
#endif

__END_DECLS
__POP_FC_STDLIB
#endif

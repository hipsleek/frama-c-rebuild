/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_LOCALE_T_H
#define __FC_DEFINE_LOCALE_T_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
struct __fc_locale_struct
{
  const char *names[13];
};

typedef struct __fc_locale_struct *locale_t;
__END_DECLS
__POP_FC_STDLIB
#endif

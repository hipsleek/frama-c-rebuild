/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_MONETARY_H
#define __FC_MONETARY_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_locale_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_ssize_t.h"

__BEGIN_DECLS

/*@
  assigns s[0 .. maxsize-1] \from format[0..];
  assigns \result \from indirect:format[0..];
*/
extern ssize_t strfmon(char *restrict s, size_t maxsize,
                       const char *restrict format, ...);

/*@
  assigns s[0 .. maxsize-1] \from format[0..], indirect:locale;
  assigns \result \from indirect:format[0..], indirect:locale;
*/
extern ssize_t strfmon_l(char *restrict s, size_t maxsize, locale_t locale,
                         const char *restrict format, ...);

__END_DECLS

__POP_FC_STDLIB
#endif

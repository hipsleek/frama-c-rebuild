/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "locale.h"
#include "limits.h"
__PUSH_FC_STDLIB
struct lconv __C_locale = {(char*)".",(char*)"",(char*)"",(char*)"",(char*)"",
                           (char*)"",(char*)"",(char*)"",(char*)"",(char*)"",
                           CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,
                           CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX,
                           CHAR_MAX,CHAR_MAX,CHAR_MAX,CHAR_MAX};

struct lconv *__frama_c_locale=&__C_locale;

const char *__frama_c_locale_names[512] = {"C"};
char *setlocale(int category, const char *locale) {
  if (*locale == 'C') 
    { __frama_c_locale = &__C_locale;
      return (char*)__frama_c_locale_names[0];
    };
  return NULL;
}

struct lconv *localeconv(void) {
  return __frama_c_locale;
}

__POP_FC_STDLIB

/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

// Logic definitions related to strto* (and wcsto*) conversion functions

#ifndef __FC_STRTO_AXIOMATIC_H
#define __FC_STRTO_AXIOMATIC_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_wchar_t.h"
#include "__fc_string_axiomatic.h"

__BEGIN_DECLS

/*@
  axiomatic StrTo {
    logic ℤ str_to_integer{L}(char *s, ℤ min, ℤ max, ℤ b)
        reads s[0 .. strlen(s)];
    // Assuming that [s] points to a valid string, returns:
    // - 1 if the initial portion of [s] can be decoded as an integer in base
    //   [b], in the range [min .. max] according to the specification of
    //   strtol and similar functions (including optional whitespace,
    //   optional sign, optional prefixes, etc.);
    // - 2 if the initial portion of [s] can be decoded as an integer,
    //   but outside the range [min .. max];
    // - 0 if the initial portion of [s] cannot be converted to an integer in
    //   base [b].

    axiom StrToIntRes:
      \forall char* s, ℤ min, max, b; valid_read_string(s) ==>
        0 <= str_to_integer(s, min, max, b) <= 2;

    logic ℤ wcs_to_integer{L}(wchar_t *s, ℤ min, ℤ max, ℤ b)
        reads s[0 .. wcslen(s)];
    // Behaves just as [str_to_integer], but for wide strings

    axiom WcsToIntRes:
      \forall wchar_t* s, ℤ min, max, b; valid_read_wstring(s) ==>
        0 <= wcs_to_integer(s, min, max, b) <= 2;
  }
*/

__END_DECLS

__POP_FC_STDLIB
#endif

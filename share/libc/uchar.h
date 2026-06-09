/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/* C11 */
#ifndef __FC_UCHAR_H
#define __FC_UCHAR_H

#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_mbstate_t.h"
#include "__fc_define_size_t.h"
#include <errno.h>

__BEGIN_DECLS

#define __STDC_VERSION_UCHAR_H__ 202311L

#ifndef __cpp_char8_t
typedef unsigned char char8_t;
#endif

#ifndef __USE_ISOCXX11
typedef __UINT_LEAST16_T char16_t;
typedef __UINT_LEAST16_T char32_t;
#endif

/* POSIX 2024: "Inclusion of the <uchar.h> header may make visible all symbols
   from the headers <stddef.h>, <stdint.h>, and <wchar.h>.
   Note: coreutils needs this: it uses wint_t without including wchar.h. */
#include <stddef.h>
#include <stdint.h>
#include <wchar.h>

/* C23 7.30.1 "...If ps is a null pointer, each function uses its own
   internal mbstate_t object..."
*/
__FC_EXTERN mbstate_t __fc_mbrtoc8_internal_mbstate;
__FC_EXTERN mbstate_t __fc_c8rtomb_internal_mbstate;
__FC_EXTERN mbstate_t __fc_mbrtoc16_internal_mbstate;
__FC_EXTERN mbstate_t __fc_c16rtomb_internal_mbstate;
__FC_EXTERN mbstate_t __fc_mbrtoc32_internal_mbstate;
__FC_EXTERN mbstate_t __fc_c32rtomb_internal_mbstate;

/* C23 7.30.1.2 / 7.30.1.4 / 7.30.1.6 : If s is a null pointer, the c8rtomb,
   c16rtomb and c32rtomb function are using an internal buffer instead.
*/
__FC_EXTERN char __fc_c8rtomb_internal_buf[__FC_MB_CUR_MAX];
__FC_EXTERN char __fc_c16rtomb_internal_buf[__FC_MB_CUR_MAX];
__FC_EXTERN char __fc_c32rtomb_internal_buf[__FC_MB_CUR_MAX];

/*@
  assigns pc8[0], *ps, __fc_mbrtoc8_internal_mbstate
    \from indirect:pc8, s[0 .. n-1], indirect:n, indirect:ps, *ps,
          __fc_mbrtoc8_internal_mbstate;
  assigns \result, errno \from indirect:s[0 .. n-1], indirect:n, indirect:ps,
    indirect:*ps, indirect:__fc_mbrtoc8_internal_mbstate;
*/
extern size_t mbrtoc8(char8_t *restrict pc8, const char *restrict s, size_t n,
                      mbstate_t *restrict ps);

/*@
  assigns s[0 .. __FC_MB_CUR_MAX-1], *ps, __fc_c8rtomb_internal_mbstate,
    __fc_c8rtomb_internal_buf[0 .. __FC_MB_CUR_MAX-1]
    \from indirect:s, c8, indirect:ps, *ps, __fc_c8rtomb_internal_mbstate,
    __fc_c8rtomb_internal_buf[0 .. __FC_MB_CUR_MAX-1];
  assigns \result, errno \from indirect:c8, indirect:ps,
    indirect:*ps, indirect:__fc_c8rtomb_internal_mbstate;
*/
extern size_t c8rtomb(char *restrict s, char8_t c8, mbstate_t *restrict ps);

/*@
  assigns pc16[0], *ps, __fc_mbrtoc16_internal_mbstate
    \from indirect:pc16, s[0 .. n-1], indirect:n, indirect:ps, *ps,
          __fc_mbrtoc16_internal_mbstate;
  assigns \result, errno \from indirect:s[0 .. n-1], indirect:n, indirect:ps,
    indirect:*ps, indirect:__fc_mbrtoc16_internal_mbstate;
*/
extern size_t mbrtoc16(char16_t *restrict pc16, const char *restrict s,
                       size_t n, mbstate_t *restrict ps);

/*@
  assigns s[0 .. __FC_MB_CUR_MAX-1], *ps, __fc_c16rtomb_internal_mbstate,
    __fc_c16rtomb_internal_buf[0 .. __FC_MB_CUR_MAX-1]
    \from indirect:s, c16, indirect:ps, *ps, __fc_c16rtomb_internal_mbstate,
    __fc_c16rtomb_internal_buf[0 .. __FC_MB_CUR_MAX-1];
  assigns \result, errno \from indirect:c16, indirect:ps,
    indirect:*ps, indirect:__fc_c16rtomb_internal_mbstate;
*/
extern size_t c16rtomb(char *restrict s, char16_t c16, mbstate_t *restrict ps);


/*@
  assigns pc32[0], *ps, __fc_mbrtoc32_internal_mbstate
    \from indirect:pc32, s[0 .. n-1], indirect:n, indirect:ps, *ps,
          __fc_mbrtoc32_internal_mbstate;
  assigns \result, errno \from indirect:s[0 .. n-1], indirect:n, indirect:ps,
    indirect:*ps, indirect:__fc_mbrtoc32_internal_mbstate;
*/
extern size_t mbrtoc32(char32_t *restrict pc32, const char *restrict s,
                       size_t n, mbstate_t *restrict ps);
/*@
  assigns s[0 .. __FC_MB_CUR_MAX-1], *ps, __fc_c32rtomb_internal_mbstate,
    __fc_c32rtomb_internal_buf[0 .. __FC_MB_CUR_MAX-1]
    \from indirect:s, c32, indirect:ps, *ps, __fc_c32rtomb_internal_mbstate,
    __fc_c32rtomb_internal_buf[0 .. __FC_MB_CUR_MAX-1];
  assigns \result, errno \from indirect:c32, indirect:ps,
    indirect:*ps, indirect:__fc_c32rtomb_internal_mbstate;
*/
extern size_t c32rtomb(char *restrict s, char32_t c32, mbstate_t *restrict ps);

__END_DECLS

__POP_FC_STDLIB
#endif

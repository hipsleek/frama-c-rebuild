/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/* ISO C: 7.26 */
#ifndef __FC_WCTYPE_H
#define __FC_WCTYPE_H

#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_locale_t.h"
#include "__fc_define_weof.h"
#include <wchar.h>
__BEGIN_DECLS

typedef const int32_t *wctrans_t;

typedef unsigned long wctype_t;

/*@
  assigns \result \from wc;
*/
extern int iswalnum(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswalpha(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswascii(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswblank(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswcntrl(wint_t wc);

/*@
  assigns \result \from wc, indirect:charclass;
*/
extern int iswctype(wint_t wc, wctype_t charclass);

/*@
  assigns \result \from wc, indirect:charclass, indirect:locale;
*/
int iswctype_l(wint_t wc, wctype_t charclass, locale_t locale);

/*@
  assigns \result \from wc;
*/
extern int iswdigit(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswgraph(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswhexnumber(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswideogram(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswlower(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswnumber(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswphonogram(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswprint(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswpunct(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswrune(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswspace(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswspecial(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswupper(wint_t wc);

/*@
  assigns \result \from wc;
*/
extern int iswxdigit(wint_t wc);

/*@
  requires valid_property: valid_read_string(property);
  assigns \result \from indirect:property[0..strlen(property)];
*/
wctype_t wctype(const char *property);

/*@
  requires valid_property: valid_read_string(property);
  assigns \result \from indirect:property[0..strlen(property)],
                        indirect:locale;
*/
wctype_t wctype_l(const char *property, locale_t locale);

/*@
  assigns \result \from wc, indirect:desc;
*/
wint_t towctrans(wint_t wc, wctrans_t desc);

/*@
  assigns \result \from wc, indirect:desc, indirect:locale;
*/
wint_t towctrans_l(wint_t wc, wctrans_t desc, locale_t locale);

/*@
  assigns \result \from wc;
*/
wint_t towlower(wint_t wc);

/*@
  assigns \result \from wc, indirect:locale;
*/
wint_t towupper(wint_t wc, locale_t locale);

/*@
  assigns \result \from indirect:charclass;
*/
wctrans_t wctrans(const char *charclass);

/*@
  assigns \result \from indirect:charclass, indirect:locale;
*/
wctrans_t wctrans_l(const char *charclass, locale_t locale);

__END_DECLS

__POP_FC_STDLIB
#endif

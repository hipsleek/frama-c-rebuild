/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/* ISO C: 7.25 */
#ifndef __FC_WCHAR_H
#define __FC_WCHAR_H

#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_mbstate_t.h"
#include "__fc_define_wchar_t.h"
#include "__fc_define_weof.h"
#include "__fc_define_wint_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_file.h"
#include "__fc_string_axiomatic.h"

// Include <stdint.h> to retrieve definitions such as WCHAR_MIN and WINT_MAX,
// required by ISO C (and not necessarily respected by the glibc).
// Note that POSIX does not specify that all symbols in <stdint.h> can be
// made visible in wchar.h, but in practice this should be fine.
#include <stdint.h>

// ISO C requires the tag 'struct tm' (as declared in <time.h>) to be declared.
#include <time.h>

#include <string.h>
#include <stdarg.h>

#include <locale.h> // for locale_t

__BEGIN_DECLS

/*@
  requires valid:
    valid_read_or_empty((char*)s, (size_t)(sizeof(wchar_t)*n))
    || \valid_read(((unsigned char*)s)+(0..wmemchr_off(s,c,n)));
  @ requires initialization:
        \initialized(s+(0..n - 1))
     || \initialized(s+(0..wmemchr_off(s,c,n)));
  @ requires danglingness:
        non_escaping(s, (size_t)(sizeof(wchar_t)*n))
     || non_escaping(s, (size_t)(sizeof(wchar_t)*(wmemchr_off(s,c,n)+1)));
  assigns \result \from s, indirect:s[0 .. n-1], indirect:c, indirect:n;
  ensures result_null_or_inside_s:
    \result == \null || \subset (\result, s+(0 .. n-1));
 */
extern wchar_t * wmemchr(const wchar_t *s, wchar_t c, size_t n);

/*@
  requires valid_s1: valid_read_or_empty(s1, (size_t)(sizeof(wchar_t)*n));
  requires valid_s2: valid_read_or_empty(s2, (size_t)(sizeof(wchar_t)*n));
  requires initialization:s1: \initialized(s1+(0..n-1));
  requires initialization:s2: \initialized(s2+(0..n-1));
  requires danglingness:s1: non_escaping(s1, (size_t)(sizeof(wchar_t)*n));
  requires danglingness:s2: non_escaping(s2, (size_t)(sizeof(wchar_t)*n));
  assigns \result \from indirect:s1[0 .. n-1], indirect:s2[0 .. n-1], indirect:n;
*/
extern int wmemcmp(const wchar_t *s1, const wchar_t *s2, size_t n);

/*@
  requires valid_dest: valid_or_empty(dest, (size_t)(sizeof(wchar_t)*n));
  requires valid_src: valid_read_or_empty(src, (size_t)(sizeof(wchar_t)*n));
  requires separation:dest:src: \separated(dest+(0 .. n-1), src+(0 .. n-1));
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
 */
extern wchar_t * wmemcpy(wchar_t *restrict dest, const wchar_t *restrict src, size_t n);

/*@
  requires valid_src: \valid_read(src+(0..n-1));
  requires valid_dest: \valid(dest+(0..n-1));
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
*/
extern wchar_t * wmemmove(wchar_t *dest, const wchar_t *src, size_t n);

/*@
  requires valid_wcs: \valid(wcs+(0..n-1));
  assigns wcs[0 .. n-1] \from wc, indirect:n;
  assigns \result \from wcs;
  ensures result_ptr: \result == wcs;
  ensures initialization:wcs: \initialized(wcs + (0 .. n-1));
  ensures contents_equal_wc: \subset(wcs[0 .. n-1], wc);
*/
extern wchar_t * wmemset(wchar_t *wcs, wchar_t wc, size_t n);

/*@
  requires valid_wstring_src: valid_read_wstring(src);
  requires valid_wstring_dest: valid_wstring(dest);
  requires room_for_concatenation: \valid(dest+(wcslen(dest)..wcslen(dest)+wcslen(src)));
  requires separation:\separated(dest+(0..wcslen(dest)+wcslen(src)),src+(0..wcslen(src)));
  assigns dest[0 .. ] \from dest[0 .. ], indirect:dest, src[0 .. ], indirect:src;
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
*/
extern wchar_t * wcscat(wchar_t *restrict dest, const wchar_t *restrict src);

/*@
  requires valid_wstring_src: valid_read_wstring(wcs);
  assigns \result \from wcs, indirect:wcs[0 ..], indirect:wc;
  ensures result_null_or_inside_wcs:
    \result == \null || \subset(\result, wcs+(0..));
*/
extern wchar_t * wcschr(const wchar_t *wcs, wchar_t wc);

/*@
  requires valid_wstring_s1: valid_read_wstring(s1); // over-strong
  requires valid_wstring_s2: valid_read_wstring(s2); // over-strong
  assigns \result \from indirect:s1[0 .. ], indirect:s2[0 .. ];
*/
extern int wcscmp(const wchar_t *s1, const wchar_t *s2);

/*@
  requires valid_wstring_src: valid_read_wstring(src);
  requires room_wstring: \valid(dest+(0 .. wcslen(src)));
  requires separation:\separated(dest+(0..wcslen(src)),src+(0..wcslen(src)));
  assigns dest[0 .. wcslen(src)] \from src[0 .. wcslen(src)], indirect:src;
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
 */
extern wchar_t * wcscpy(wchar_t *restrict dest, const wchar_t *restrict src);

/*@
  requires valid_wstring_wcs: valid_read_wstring(wcs);
  requires valid_wstring_accept: valid_read_wstring(accept);
  assigns \result \from indirect:wcs[0 .. ], indirect:accept[0 .. ];
 */
extern size_t wcscspn(const wchar_t *wcs, const wchar_t *accept);

// wcslcat is a BSD extension (non-C99, non-POSIX)
/*@
  requires valid_nwstring_src: valid_read_nwstring(src, n);
  requires valid_wstring_dest: valid_wstring(dest);
  requires room_for_concatenation: \valid(dest+(wcslen(dest)..wcslen(dest)+\min(wcslen(src), n)));
  requires separation:\separated(dest+(0..wcslen(dest)+wcslen(src)),src+(0..wcslen(src)));
  assigns dest[0 .. ] \from dest[0 .. ], indirect:dest, src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from indirect:dest[0 .. ], indirect:src[0 .. n-1], indirect:n;
*/
extern size_t wcslcat(wchar_t *restrict dest, const wchar_t *restrict src, size_t n);

// wcslcpy is a BSD extension (non-C99, non-POSIX)
/*@
  requires valid_wstring_src: valid_read_wstring(src);
  requires room_nwstring: \valid(dest+(0 .. n));
  requires separation:dest:src: \separated(dest+(0 .. n-1), src+(0 .. n-1));
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from indirect:dest[0 .. n-1], indirect:dest,
    indirect:src[0 .. n-1], indirect:src, indirect:n;
 */
extern size_t wcslcpy(wchar_t *dest, const wchar_t *src, size_t n);

/*@
  requires valid_string_s: valid_read_wstring(s);
  assigns \result \from indirect:s[0 .. wcslen(s)];
  ensures result_is_length: \result == wcslen(s);
 */
extern size_t wcslen(const wchar_t *s);

/*@
  requires valid_nwstring_src: valid_read_nwstring(src, n);
  requires valid_wstring_dest: valid_wstring(dest);
  requires room_for_concatenation: \valid(dest+(wcslen(dest)..wcslen(dest)+\min(wcslen(src), n)));
  requires separation:\separated(dest+(0..wcslen(dest)+wcslen(src)),src+(0..wcslen(src)));
  assigns dest[0 .. ] \from dest[0 .. ], indirect:dest, src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
*/
extern wchar_t * wcsncat(wchar_t *restrict dest, const wchar_t *restrict src, size_t n);

/*@
  requires valid_wstring_s1: valid_read_wstring(s1); // over-strong
  requires valid_wstring_s2: valid_read_wstring(s2); // over-strong
  assigns \result \from indirect:s1[0 .. n-1], indirect:s2[0 .. n-1], indirect:n;
*/
extern int wcsncmp(const wchar_t *s1, const wchar_t *s2, size_t n);

/*@
  requires valid_wstring_src: valid_read_wstring(src);
  requires room_nwstring: \valid(dest+(0 .. n-1));
  requires separation:dest:src: \separated(dest+(0 .. n-1), src+(0 .. n-1));
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:src, indirect:n;
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
  ensures initialization: \initialized(dest+(0 .. n-1));
 */
extern wchar_t * wcsncpy(wchar_t *restrict dest, const wchar_t *restrict src, size_t n);

/*@
  requires valid_wstring_wcs: valid_read_wstring(wcs);
  requires valid_wstring_accept: valid_read_wstring(accept);
  assigns \result \from wcs, indirect:wcs[0 .. ], indirect:accept[0 .. ];
  ensures result_null_or_inside_wcs:
    \result == \null || \subset (\result, wcs+(0 .. ));
*/
extern wchar_t * wcspbrk(const wchar_t *wcs, const wchar_t *accept);

/*@
  requires valid_wstring_wcs: valid_read_wstring(wcs);
  assigns \result \from wcs, indirect:wcs[0 .. wcslen(wcs)], indirect:wc;
  ensures result_null_or_inside_wcs:
    \result == \null || \subset (\result, wcs+(0 .. ));
 */
extern wchar_t * wcsrchr(const wchar_t *wcs, wchar_t wc);

/*@
  requires valid_wstring_wcs: valid_read_wstring(wcs);
  requires valid_wstring_accept: valid_read_wstring(accept);
  assigns \result \from indirect:wcs[0 .. wcslen(wcs)],
                        indirect:accept[0 .. wcslen(accept)];
*/
extern size_t wcsspn(const wchar_t *wcs, const wchar_t *accept);

/*@
  requires valid_wstring_haystack: valid_read_wstring(haystack);
  requires valid_wstring_needle: valid_read_wstring(needle);
  assigns \result \from haystack, indirect:haystack[0 .. ], indirect:needle[0 .. ];
  ensures result_null_or_inside_haystack:
    \result == \null || \subset (\result, haystack+(0 .. ));
 */
extern wchar_t * wcsstr(const wchar_t *haystack, const wchar_t *needle);

/*@
  requires room_nwstring: \valid(ws+(0..n-1));
  requires valid_stream: \valid(stream);
  assigns ws[0..n-1] \from indirect:n, indirect:*stream;
  assigns \result \from ws, indirect:n, indirect:*stream;
  ensures result_null_or_same: \result == \null || \result == ws;
  ensures terminated_string_on_success:
    \result != \null ==> valid_wstring(ws);
 */
extern wchar_t *fgetws(wchar_t * restrict ws, int n, FILE * restrict stream);

/*@
  // Axiomatic used by the Variadic module to generate specifications
  // for some functions, e.g. swprintf().
  axiomatic wformat_length {
    //TODO: this logic function will be extended to handle variadic formats
    logic integer wformat_length{L}(wchar_t *format);
  }
*/

/*@
  //missing: assigns \from 'current locale'
  requires valid_wstring_ws1: valid_read_wstring(ws1);
  requires valid_wstring_ws2: valid_read_wstring(ws2);
  assigns \result \from indirect:ws1[0..], indirect:ws2[0..];
*/
extern int wcscasecmp(const wchar_t *ws1, const wchar_t *ws2);

/*@
  requires valid_wstring: valid_read_wstring(ws);
  allocates \result;
  assigns \result \from indirect:ws[0..wcslen(ws)], indirect:__fc_heap_status;
  assigns __fc_heap_status \from indirect:ws[0 .. wcslen(ws)],
                                 __fc_heap_status;
  behavior allocation:
    assumes can_allocate: is_allocable(wcslen(ws));
    assigns __fc_heap_status \from indirect:ws[0 .. wcslen(ws)],
                                   __fc_heap_status;
    assigns \result \from indirect:ws[0..wcslen(ws)], indirect:__fc_heap_status;
    ensures allocation: \fresh(\result,wcslen(ws) * sizeof(wchar_t));
    ensures result_valid_string_and_same_contents:
      valid_wstring(\result) && wcscmp(\result,ws) == 0;
  behavior no_allocation:
    assumes cannot_allocate: !is_allocable(wcslen(ws));
    allocates \nothing;
    assigns \result \from \nothing;
    ensures result_null: \result == \null;
*/
extern wchar_t *wcsdup(const wchar_t *ws);

/*@
  requires valid_mbstate:initialization:
    ps == \null || (\valid_read(ps) && \initialized(ps));
  assigns \result \from indirect:ps, indirect:*ps;
  ensures ok_or_error: \result >= 0;
*/
extern int mbsinit(const mbstate_t *ps);

/*@
  assigns \result, *pwc, *ps \from indirect:s[0 .. n], indirect:n, indirect:ps;
*/
extern size_t mbrtowc(wchar_t *restrict pwc, const char *restrict s, size_t n,
                      mbstate_t *restrict ps);

#include "__fc_strto_axiomatic.h"

/*@
  requires valid_string_nptr: valid_read_wstring(nptr);
  requires separation: \separated(nptr, endptr);
  requires base_range: base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                        indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                        indirect:endptr, indirect:base;
  assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
  behavior cannot_convert:
    assumes
      no_conversion: wcs_to_integer(nptr, LONG_MIN, LONG_MAX, base) == 0;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
    ensures result_no_conversion: \result == 0;
    ensures errno_set: __fc_errno == EINVAL;
  behavior out_of_range_null_endptr:
    assumes
      out_of_range: wcs_to_integer(nptr, LONG_MIN, LONG_MAX, base) == 2;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
    ensures result_out_of_range: \result == LONG_MIN || \result == LONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior out_of_range_nonnull_endptr:
    // Note: the standard does not state that endptr is definitively assigned
    //       to in this case, so we assume it may be.
    assumes
      out_of_range: wcs_to_integer(nptr, LONG_MIN, LONG_MAX, base) == 2;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
    assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
    ensures result_out_of_range: \result == LONG_MIN || \result == LONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior in_range_null_endptr:
    assumes in_range: wcs_to_integer(nptr, LONG_MIN, LONG_MAX, base) == 1;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
  behavior in_range_nonnull_endptr:
    assumes in_range: wcs_to_integer(nptr, LONG_MIN, LONG_MAX, base) == 1;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
  complete behaviors;
  disjoint behaviors;
*/
long wcstol(const wchar_t *restrict nptr, wchar_t **restrict endptr,
            int base);

/*@
  requires valid_string_nptr: valid_read_wstring(nptr);
  requires separation: \separated(nptr, endptr);
  requires base_range: base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                        indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                        indirect:endptr, indirect:base;
  assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
  behavior cannot_convert:
    assumes
      no_conversion: wcs_to_integer(nptr, LLONG_MIN, LLONG_MAX, base) == 0;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
    ensures result_no_conversion: \result == 0;
    ensures errno_set: __fc_errno == EINVAL;
  behavior out_of_range_null_endptr:
    assumes
      out_of_range: wcs_to_integer(nptr, LLONG_MIN, LLONG_MAX, base) == 2;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
    ensures result_out_of_range: \result == LLONG_MIN || \result == LLONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior out_of_range_nonnull_endptr:
    // Note: the standard does not state that endptr is definitively assigned
    //       to in this case, so we assume it may be.
    assumes
      out_of_range: wcs_to_integer(nptr, LLONG_MIN, LLONG_MAX, base) == 2;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
    assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
    ensures result_out_of_range: \result == LLONG_MIN || \result == LLONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior in_range_null_endptr:
    assumes in_range: wcs_to_integer(nptr, LLONG_MIN, LLONG_MAX, base) == 1;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
  behavior in_range_nonnull_endptr:
    assumes in_range: wcs_to_integer(nptr, LLONG_MIN, LLONG_MAX, base) == 1;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
  complete behaviors;
  disjoint behaviors;
*/
long long wcstoll(const wchar_t *restrict nptr,
                  wchar_t **restrict endptr, int base);

/*@
  requires valid_string_nptr: valid_read_wstring(nptr);
  requires separation: \separated(nptr, endptr);
  requires base_range: base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                        indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                        indirect:endptr, indirect:base;
  assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
  behavior cannot_convert:
    assumes
      no_conversion: wcs_to_integer(nptr, 0, ULONG_MAX, base) == 0;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
    ensures result_no_conversion: \result == 0;
    ensures errno_set: __fc_errno == EINVAL;
  behavior out_of_range_null_endptr:
    assumes
      out_of_range: wcs_to_integer(nptr, 0, ULONG_MAX, base) == 2;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
    ensures result_out_of_range: \result == ULONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior out_of_range_nonnull_endptr:
    // Note: the standard does not state that endptr is definitively assigned
    //       to in this case, so we assume it may be.
    assumes
      out_of_range: wcs_to_integer(nptr, 0, ULONG_MAX, base) == 2;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
    assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
    ensures result_out_of_range: \result == ULONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior in_range_null_endptr:
    assumes in_range: wcs_to_integer(nptr, 0, ULONG_MAX, base) == 1;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
  behavior in_range_nonnull_endptr:
    assumes in_range: wcs_to_integer(nptr, 0, ULONG_MAX, base) == 1;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
  complete behaviors;
  disjoint behaviors;
*/
unsigned long wcstoul(const wchar_t *restrict nptr,
                      wchar_t **restrict endptr, int base);

/*@
  requires valid_string_nptr: valid_read_wstring(nptr);
  requires separation: \separated(nptr, endptr);
  requires base_range: base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                        indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                        indirect:endptr, indirect:base;
  assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
  behavior cannot_convert:
    assumes
      no_conversion: wcs_to_integer(nptr, 0, ULLONG_MAX, base) == 0;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
    ensures result_no_conversion: \result == 0;
    ensures errno_set: __fc_errno == EINVAL;
  behavior out_of_range_null_endptr:
    assumes
      out_of_range: wcs_to_integer(nptr, 0, ULLONG_MAX, base) == 2;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
    ensures result_out_of_range: \result == ULLONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior out_of_range_nonnull_endptr:
    // Note: the standard does not state that endptr is definitively assigned
    //       to in this case, so we assume it may be.
    assumes
      out_of_range: wcs_to_integer(nptr, 0, ULLONG_MAX, base) == 2;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
    assigns __fc_errno \from indirect:nptr[0 .. wcslen(nptr)], indirect:base;
    ensures result_out_of_range: \result == ULLONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior in_range_null_endptr:
    assumes in_range: wcs_to_integer(nptr, 0, ULLONG_MAX, base) == 1;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
  behavior in_range_nonnull_endptr:
    assumes in_range: wcs_to_integer(nptr, 0, ULLONG_MAX, base) == 1;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
  complete behaviors;
  disjoint behaviors;
*/
unsigned long long wcstoull(const wchar_t *restrict nptr,
                            wchar_t **restrict endptr, int base);

/*@
  assigns \result \from c;
*/
wint_t btowc(int c);

/*@
  assigns \result, *stream \from wc, *stream;
*/
wint_t fputwc(wchar_t wc, FILE *stream);

/*@
  assigns \result, *stream \from ws[0..], *stream;
*/
int fputws(const wchar_t *restrict ws, FILE *restrict stream);

/*@
  assigns \result, *stream \from *stream, mode;
*/
int fwide(FILE *stream, int mode);

/*@
  assigns \result, *stream \from *stream;
*/
wint_t fgetwc(FILE *stream);

/*@
  assigns \result, *stream \from *stream;
*/
wint_t getwc(FILE *stream);

/*@
  //missing: assigns \result, *__fc_stdin \from *__fc_stdin;
  assigns \result \from \nothing;
*/
wint_t getwchar(void);

/*@
  assigns \result, *stream \from wc, *stream;
*/
wint_t ungetwc(wint_t wc, FILE *stream);

/*@
  assigns \result, *ps \from s[0 .. n-1], *ps;
*/
size_t mbrlen(const char *restrict s, size_t n, mbstate_t *restrict ps);

/*@
  assigns \result, dst[0 .. len-1], *ps \from (*src)[0 .. nms-1], *ps;
*/
size_t mbsnrtowcs(wchar_t *restrict dst, const char **restrict src, size_t nms,
                  size_t len, mbstate_t *restrict ps);

/*@
  assigns \result, dst[0 .. len-1], *ps \from (*src)[0 .. ], *ps;
*/
size_t mbsrtowcs(wchar_t *restrict dst, const char **restrict src, size_t len,
                 mbstate_t *restrict ps);

//requires access to __fc_fopen from stdio.h
//FILE *open_wmemstream(wchar_t **bufp, size_t *sizep);

/*@
  assigns \result, *stream \from wc, *stream;
*/
wint_t putwc(wchar_t wc, FILE *stream);

/*@
  //missing: assigns \result, *__fc_stdout \from *__fc_stdout, wc;
  assigns \result \from wc;
*/
wint_t putwchar(wchar_t wc);

/*@
  assigns *stream \from format[0 .. wcslen(format)], indirect: args;
  assigns \result \from indirect:format[0 .. wcslen(format)], indirect:args;
*/
int vfwprintf(FILE *restrict stream, const wchar_t *restrict format,
              va_list args);

/*@
  assigns *stream \from format[0 .. wcslen(format)], *stream;
  // missing: assign args
*/
int vfwscanf(FILE *restrict stream, const wchar_t *restrict format,
             va_list args);

/*@
  assigns wcs[0 .. maxlen-1] \from format[0 .. wcslen(format)],
                                   indirect:maxlen, args;
*/
int vswprintf(wchar_t *restrict wcs, size_t maxlen,
              const wchar_t *restrict format, va_list args);

/*@ assigns \result \from indirect:stream[0..], indirect:format[0..],
                          indirect:args; //missing: 'assigns args' */
int vswscanf(const wchar_t *restrict stream, const wchar_t *restrict format,
             va_list args);

/*@
  //missing:assigns *__fc_stdout \from format[0 .. wcslen(format)], indirect: args;
  assigns \result \from indirect:format[0 .. wcslen(format)], indirect:args;
*/
int vwprintf(const wchar_t *restrict format, va_list args);

/*@
  // missing: assigns args; assigns *__fc_stdin \from *__fc_stdin;
  assigns \result \from indirect:format[0 .. wcslen(format)],
                        indirect:args;
*/
int vwscanf(const wchar_t *restrict format, va_list args);

/*@
  assigns \result \from ws1;
  assigns ws1[0 ..] \from ws2[0 ..];
*/
wchar_t *wcpcpy(wchar_t *restrict ws1, const wchar_t *restrict ws2);

/*@
  assigns \result \from ws1, indirect:n;
  assigns ws1[0 .. n-1] \from ws2[0 .. n-1];
*/
wchar_t *wcpncpy(wchar_t *restrict ws1, const wchar_t *restrict ws2, size_t n);

/*@
  assigns s[0..], *ps \from wc, *ps;
*/
size_t wcrtomb(char *restrict s, wchar_t wc, mbstate_t *restrict ps);

/*@
  assigns \result \from indirect:ws1[0..], indirect:ws2[0..], indirect:locale;
*/
int wcscasecmp_l(const wchar_t *ws1, const wchar_t *ws2, locale_t locale);

/*@
  //missing: assigns \from 'current locale'
  assigns \result \from indirect:ws1[0 .. ], indirect:ws2[0 .. ];
*/
int wcscoll(const wchar_t *ws1, const wchar_t *ws2);

/*@
  assigns \result \from indirect:ws1[0 .. ], indirect:ws2[0 .. ],
                        indirect:locale;
*/
int wcscoll_l(const wchar_t *ws1, const wchar_t *ws2, locale_t locale);

/*@
  assigns wcs[0 .. maxsize-1] \from indirect:maxsize,
                                    indirect:format[0 .. wcslen(format)],
                                    indirect:*timeptr;
  assigns \result \from indirect:maxsize,
                        indirect:format[0 .. wcslen(format)],
                        indirect:*timeptr;
*/
size_t wcsftime(wchar_t *restrict wcs, size_t maxsize,
                const wchar_t *restrict format,
                const struct tm *restrict timeptr);

/*@
  //missing: assigns \from 'current locale'
  assigns \result \from indirect:ws1[0 .. n-1], indirect:ws2[0 ..  n-1], n;
*/
int wcsncasecmp(const wchar_t *ws1, const wchar_t *ws2, size_t n);

/*@
  assigns \result \from indirect:ws1[0 .. n-1], indirect:ws2[0 ..  n-1], n,
                        indirect:locale;
*/
int wcsncasecmp_l(const wchar_t *ws1, const wchar_t *ws2, size_t n,
                  locale_t locale);

/*@
  assigns \result \from ws[0 .. maxlen-1];
*/
size_t wcsnlen(const wchar_t *ws, size_t maxlen);

/*@
  assigns dst[0 .. len-1], *ps \from src[0 .. nwc-1], indirect:nwc,
                                     indirect:len, *ps;
*/
size_t wcsnrtombs(char *restrict dst, const wchar_t **restrict src, size_t nwc,
                  size_t len, mbstate_t *restrict ps);

/*@
  assigns dst[0 .. len-1], *ps \from (*src)[0 .. wcslen(*src)-1], indirect:len,
                                     *ps;
*/
size_t wcsrtombs(char *restrict dst, const wchar_t **restrict src, size_t len,
                 mbstate_t *restrict ps);

/*@
  assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)];
  assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                              indirect:endptr;
*/
double wcstod(const wchar_t *restrict nptr, wchar_t **restrict endptr);

/*@
  assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)];
  assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                              indirect:endptr;
*/
float wcstof(const wchar_t *restrict nptr, wchar_t **restrict endptr);

/*@
  assigns ws[0..] \from ws[0..],
      indirect:ws, indirect:*saveptr, indirect:delim[0..wcslen(delim)];
  assigns (*saveptr)[0..] \from (*saveptr)[0..],
      indirect:ws, indirect:*saveptr, indirect:delim[0..wcslen(delim)];
  assigns \result \from ws, *saveptr, indirect:ws[0..],
      indirect:(*saveptr)[0..], indirect:delim[0..wcslen(delim)];
  assigns *saveptr \from \old(*saveptr), ws,
                         indirect:(*saveptr)[0..],
                         indirect:delim[0..wcslen(delim)];
*/
wchar_t *wcstok(wchar_t *restrict ws, const wchar_t *restrict delim,
                wchar_t **restrict saveptr);

/*@
  assigns \result \from indirect:nptr, indirect:nptr[0 .. wcslen(nptr)];
  assigns *endptr \from nptr, indirect:nptr[0 .. wcslen(nptr)],
                              indirect:endptr;
*/
long double wcstold(const wchar_t *restrict nptr, wchar_t **restrict endptr);

/*@
  assigns \result \from pwcs;
  ensures result_minus_one_or_null_or_width: \result >= -1;
*/
int wcswidth(const wchar_t *pwcs, size_t n);

/*@
  //missing: assigns \from 'current locale'
  assigns \result, *ws1 \from ws2[0 .. n-1], indirect:n;
*/
size_t wcsxfrm(wchar_t *restrict ws1, const wchar_t *restrict ws2, size_t n);

/*@
  assigns \result, *ws1 \from ws2[0 .. n-1], indirect:n, indirect:locale;
*/
size_t wcsxfrm_l(wchar_t *restrict ws1, const wchar_t *restrict ws2, size_t n,
                 locale_t locale);

/*@
  assigns \result \from c;
*/
int wctob(wint_t c);

/* It is unclear whether these are more often in wchar.h or stdio.h */

extern int fwprintf(FILE * stream, const wchar_t * format, ...);

extern int swprintf(wchar_t * ws, size_t n, const wchar_t * format, ...);

extern int wprintf(const wchar_t * format, ...);


extern int wscanf(const wchar_t * format, ...);

extern int fwscanf(FILE * stream, const wchar_t * format, ...);

extern int swscanf(const wchar_t * str, const wchar_t * format, ...);

__END_DECLS

__POP_FC_STDLIB
#endif

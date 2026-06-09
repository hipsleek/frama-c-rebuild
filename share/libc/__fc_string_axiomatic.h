/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/* $Id: jessie_machine_prolog.h,v 1.8 2008-12-09 10:17:25 uid525 Exp $ */

#ifndef __FC_STRING_AXIOMATIC_H
#define __FC_STRING_AXIOMATIC_H
#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_define_null.h"
#include "__fc_define_wchar_t.h"

__BEGIN_DECLS

/*@ axiomatic MemCmp {
  @ logic ℤ memcmp{L1,L2}(char *s1, char *s2, ℤ n)
  @   reads \at(s1[0..n - 1],L1), \at(s2[0..n - 1],L2);
  @
  @ axiom memcmp_zero{L1,L2}:
  @   \forall char *s1, *s2; \forall ℤ n;
  @      memcmp{L1,L2}(s1,s2,n) == 0
  @      <==> \forall ℤ i; 0 <= i < n ==> \at(s1[i],L1) == \at(s2[i],L2);
  @
  @ }
  @*/

/*@ axiomatic MemChr {
  @ logic 𝔹 memchr{L}(char *s, ℤ c, ℤ n)
  @   reads s[0..n - 1];
  @ // Returns [true] iff array [s] contains character [c]
  @
  @ logic ℤ memchr_off{L}(char *s, ℤ c, ℤ n)
  @   reads s[0..n - 1];
  @ // Returns the offset at which [c] appears in [s], or [n-1] otherwise.
  @ // It represents the largest searched index when looking for [c] in [s].
  @
  @ axiom memchr_def{L}:
  @   \forall char *s; \forall ℤ c; \forall ℤ n;
  @      memchr(s,c,n) <==> \exists int i; 0 <= i < n && s[i] == c;
  @ }
  @*/

/*@ axiomatic MemSet {
  @ logic 𝔹 memset{L}(char *s, ℤ c, ℤ n)
  @   reads s[0..n - 1];
  @ // Returns [true] iff array [s] contains only character [c]
  @
  @ axiom memset_def{L}:
  @   \forall char *s; \forall ℤ c; \forall ℤ n;
  @      memset(s,c,n) <==> \forall ℤ i; 0 <= i < n ==> s[i] == c;
  @ }
  @*/

/*@ axiomatic StrLen {
  @ logic ℤ strlen{L}(char *s)
  @   reads s[0..];
  @
  @ axiom strlen_pos_or_null{L}:
  @   \forall char* s; \forall ℤ i;
  @      (0 <= i
  @       && (\forall ℤ j; 0 <= j < i ==> s[j] != '\0')
  @       && s[i] == '\0') ==> strlen(s) == i;
  @
  @ axiom strlen_neg{L}:
  @   \forall char* s;
  @      (\forall ℤ i; 0 <= i ==> s[i] != '\0')
  @      ==> strlen(s) < 0;
  @
  @ axiom strlen_before_null{L}:
  @   \forall char* s; \forall ℤ i; 0 <= i < strlen(s) ==> s[i] != '\0';
  @
  @ axiom strlen_at_null{L}:
  @   \forall char* s; 0 <= strlen(s) ==> s[strlen(s)] == '\0';
  @
  @ axiom strlen_not_zero{L}:
  @   \forall char* s; \forall ℤ i;
  @      0 <= i <= strlen(s) && s[i] != '\0' ==> i < strlen(s);
  @
  @ axiom strlen_zero{L}:
  @   \forall char* s; \forall ℤ i;
  @      0 <= i <= strlen(s) && s[i] == '\0' ==> i == strlen(s);
  @
  @ axiom strlen_sup{L}:
  @   \forall char* s; \forall ℤ i;
  @      0 <= i && s[i] == '\0' ==> 0 <= strlen(s) <= i;
  @
  @ axiom strlen_shift{L}:
  @   \forall char* s; \forall ℤ i;
  @      0 <= i <= strlen(s) ==> strlen(s + i) == strlen(s) - i;
  @
  @ axiom strlen_create{L}:
  @   \forall char* s; \forall ℤ i;
  @      0 <= i && s[i] == '\0' ==> 0 <= strlen(s) <= i;
  @
  @ axiom strlen_create_shift{L}:
  @   \forall char* s; \forall ℤ i; \forall ℤ k;
  @      0 <= k <= i && s[i] == '\0' ==> 0 <= strlen(s+k) <= i - k;
  @
  @ axiom memcmp_strlen_left{L}:
  @   \forall char *s1, *s2; \forall ℤ n;
  @      memcmp{L,L}(s1,s2,n) == 0 && strlen(s1) < n ==> strlen(s1) ==
  strlen(s2);
  @
  @ axiom memcmp_strlen_right{L}:
  @   \forall char *s1, *s2; \forall ℤ n;
  @      memcmp{L,L}(s1,s2,n) == 0 && strlen(s2) < n ==> strlen(s1) ==
  strlen(s2);
  @
  @ axiom memcmp_strlen_shift_left{L}:
  @   \forall char *s1, *s2; \forall ℤ k, n;
  @      memcmp{L,L}(s1,s2 + k,n) == 0 && 0 <= k && strlen(s1) < n ==>
  @        0 <= strlen(s2) <= k + strlen(s1);
  @
  @ axiom memcmp_strlen_shift_right{L}:
  @   \forall char *s1, *s2; \forall ℤ k, n;
  @      memcmp{L,L}(s1 + k,s2,n) == 0 && 0 <= k && strlen(s2) < n ==>
  @        0 <= strlen(s1) <= k + strlen(s2);
  @ }
  @*/

/*@
  logic integer strlen_aux(char a[], integer idx) =
    idx>=\length(a)||idx<0?-1:a[idx]=='\000'?idx:strlen_aux(a,idx+1);
*/

/*@
  logic integer strlen(char a[]) = strlen_aux(a,0);
*/

/*@ axiomatic StrCmp {
  @ logic ℤ strcmp{L}(char *s1, char *s2)
  @   reads s1[0..strlen(s1)], s2[0..strlen(s2)];
  @
  @ axiom strcmp_zero{L}:
  @   \forall char *s1, *s2;
  @      strcmp(s1,s2) == 0 <==>
  @        (strlen(s1) == strlen(s2)
  @         && \forall ℤ i; 0 <= i <= strlen(s1) ==> s1[i] == s2[i]);
  @ }
  @*/

/*@ axiomatic StrNCmp {
  @ logic ℤ strncmp{L}(char *s1, char *s2, ℤ n)
  @   reads s1[0..n-1], s2[0..n-1];
  @
  @ axiom strncmp_zero{L}:
  @   \forall char *s1, *s2; \forall ℤ n;
  @      strncmp(s1,s2,n) == 0 <==>
  @        (strlen(s1) < n && strcmp(s1,s2) == 0
  @         || \forall ℤ i; 0 <= i < n ==> s1[i] == s2[i]);
  @ }
  @*/

/*@ axiomatic StrChr {
  @ logic 𝔹 strchr{L}(char *s, ℤ c)
  @   reads s[0..strlen(s)];
  @ // Returns [true] iff string [s] contains [c] (interpreted as char)
  @
  @ axiom strchr_def{L}:
  @   \forall char *s; \forall ℤ c;
  @      strchr(s,c) <==> \exists ℤ i; 0 <= i <= strlen(s) && s[i] == (char)c;
  @ }
  @*/

/*@ axiomatic WMemChr {
  @ logic 𝔹 wmemchr{L}(wchar_t *s, wchar_t c, ℤ n)
  @   reads s[0..n - 1];
  @ // Returns [true] iff wide char array [s] contains wide character [c]
  @
  @ logic ℤ wmemchr_off{L}(wchar_t *s, wchar_t c, ℤ n)
  @   reads s[0..n - 1];
  @ // Returns the offset at which [c] appears in [s].
  @
  @ axiom wmemchr_def{L}:
  @   \forall wchar_t *s; \forall wchar_t c; \forall ℤ n;
  @      wmemchr(s,c,n) <==> \exists int i; 0 <= i < n && s[i] == c;
  @ }
  @*/

/*@ axiomatic WcsLen {
  @ logic ℤ wcslen{L}(wchar_t *s)
  @   reads s[0..];
  @
  @ axiom wcslen_pos_or_null{L}:
  @   \forall wchar_t* s; \forall ℤ i;
  @      (0 <= i
  @       && (\forall ℤ j; 0 <= j < i ==> s[j] != L'\0')
  @       && s[i] == L'\0') ==> wcslen(s) == i;
  @
  @ axiom wcslen_neg{L}:
  @   \forall wchar_t* s;
  @      (\forall ℤ i; 0 <= i ==> s[i] != L'\0')
  @      ==> wcslen(s) < 0;
  @
  @ axiom wcslen_before_null{L}:
  @   \forall wchar_t* s; \forall int i; 0 <= i < wcslen(s) ==> s[i] != L'\0';
  @
  @ axiom wcslen_at_null{L}:
  @   \forall wchar_t* s; 0 <= wcslen(s) ==> s[wcslen(s)] == L'\0';
  @
  @ axiom wcslen_not_zero{L}:
  @   \forall wchar_t* s; \forall int i;
  @      0 <= i <= wcslen(s) && s[i] != L'\0' ==> i < wcslen(s);
  @
  @ axiom wcslen_zero{L}:
  @   \forall wchar_t* s; \forall int i;
  @      0 <= i <= wcslen(s) && s[i] == L'\0' ==> i == wcslen(s);
  @
  @ axiom wcslen_sup{L}:
  @   \forall wchar_t* s; \forall int i;
  @      0 <= i && s[i] == L'\0' ==> 0 <= wcslen(s) <= i;
  @
  @ axiom wcslen_shift{L}:
  @   \forall wchar_t* s; \forall int i;
  @      0 <= i <= wcslen(s) ==> wcslen(s+i) == wcslen(s)-i;
  @
  @ axiom wcslen_create{L}:
  @   \forall wchar_t* s; \forall int i;
  @      0 <= i && s[i] == L'\0' ==> 0 <= wcslen(s) <= i;
  @
  @ axiom wcslen_create_shift{L}:
  @   \forall wchar_t* s; \forall int i; \forall int k;
  @      0 <= k <= i && s[i] == L'\0' ==> 0 <= wcslen(s+k) <= i - k;
  @ }
  @*/

/*@
   logic integer wcslen_aux(wchar_t a[], integer idx) =
     idx < 0 || idx >= \length(a) ? -1 :
       a[idx] == 0 ? idx : wcslen_aux(a,idx+1);
*/

/*@ logic integer wcslen(wchar_t a[]) = wcslen_aux(a,0); */

/*@ axiomatic WcsCmp {
  @ logic ℤ wcscmp{L}(wchar_t *s1, wchar_t *s2)
  @   reads s1[0..wcslen(s1)], s2[0..wcslen(s2)];
  @
  @ axiom wcscmp_zero{L}:
  @   \forall wchar_t *s1, *s2;
  @      wcscmp(s1,s2) == 0 <==>
  @        (wcslen(s1) == wcslen(s2)
  @         && \forall ℤ i; 0 <= i <= wcslen(s1) ==> s1[i] == s2[i]);
  @ }
  @*/

/*@ axiomatic WcsNCmp {
  @ logic ℤ wcsncmp{L}(wchar_t *s1, wchar_t *s2, ℤ n)
  @   reads s1[0..n-1], s2[0..n-1];
  @
  @ axiom wcsncmp_zero{L}:
  @   \forall wchar_t *s1, *s2; \forall ℤ n;
  @      wcsncmp(s1,s2,n) == 0 <==>
  @        (wcslen(s1) < n && wcscmp(s1,s2) == 0
  @         || \forall ℤ i; 0 <= i < n ==> s1[i] == s2[i]);
  @ }
  @*/

/*@ axiomatic WcsChr {
  @ logic 𝔹 wcschr{L}(wchar_t *wcs, ℤ wc)
  @   reads wcs[0..wcslen(wcs)];
  @ // Returns [true] iff wide string [wcs] contains [wc]
  @ //(interpreted as wchar_t)
  @
  @ axiom wcschr_def{L}:
  @   \forall wchar_t *wcs; \forall ℤ wc;
  @      wcschr(wcs,wc) <==> \exists ℤ i; 0 <= i <= wcslen(wcs)
  @                                       && wcs[i] == (wchar_t)wc;
  @ }
  @*/

/*@ predicate valid_string{L}(char *s) =
  @   0 <= strlen(s) && \valid(s+(0..strlen(s)));
  @
  @ predicate valid_read_string{L}(char *s) =
  @   0 <= strlen(s) && \valid_read(s+(0..strlen(s)));
  @
  @ predicate valid_read_nstring{L}(char *s, ℤ n) =
  @   (\valid_read(s+(0..n-1)) && \initialized(s+(0..n-1)))
  @   || valid_read_string{L}(s);
  @
  @ predicate valid_string_or_null{L}(char *s) =
  @   s == \null || valid_string(s);
  @
  @ predicate valid_wstring{L}(wchar_t *s) =
  @   0 <= wcslen(s) && \valid(s+(0..wcslen(s)));
  @
  @ predicate valid_read_wstring{L}(wchar_t *s) =
  @   0 <= wcslen(s) && \valid_read(s+(0..wcslen(s)));
  @
  @ predicate valid_read_nwstring{L}(wchar_t *s, ℤ n) =
  @   (\valid_read(s+(0..n-1)) && \initialized(s+(0..n-1)))
  @   || valid_read_wstring{L}(s);
  @
  @ predicate valid_wstring_or_null{L}(wchar_t *s) =
  @   s == \null || valid_wstring(s);
  @
  @ logic ℤ strnlen(char *s, ℤ n) =
  @   \min(strlen(s), n);
  @*/

__END_DECLS

#define FRAMA_C_PTR __declspec(valid)
#define FRAMA_C_STRING __declspec(valid_string)
#define FRAMA_C_STRING_OR_NULL __declspec(valid_string_or_null)
#define FRAMA_C_WSTRING __declspec(valid_wstring)
#define FRAMA_C_WSTRING_OR_NULL __declspec(valid_wstring_or_null)

__POP_FC_STDLIB
#endif

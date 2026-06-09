/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_STRING_H
#define __FC_STRING_H

#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_string_axiomatic.h"
#include "__fc_alloc_axiomatic.h"
#include <stddef.h>
#include <limits.h>

__BEGIN_DECLS

// Query memory

/*@ predicate non_escaping{L}(void *s, integer n) =
      \forall integer i; 0 <= i < n ==> !\dangling((char *)s + i);
*/

#if __STDC_VERSION__ > 202311L
/* In C2y (post-C23), pointers are no longer required to be valid when dealing
   with a 0-byte function call:
   "Where an argument declared as size_t n specifies the length of the array
   for a function, n can have the value zero on a call to that function.
   Unless explicitly stated otherwise (...), pointer arguments on such a call
   may be null pointers.
   Therefore, we are more permissive in our conditions related to
   valid_or_empty and valid_read_or_empty.
*/
/*@
  predicate valid_or_empty{L}(void *s, size_t n) =
    (\object_pointer((char*)s) || s == \null) && \valid(((char*)s)+(0..n-1));

  predicate valid_read_or_empty{L}(void *s, size_t n) =
    (\object_pointer((char*)s) || s == \null) &&
    \valid_read(((char*)s)+(0..n-1));
*/
#else
/*@
  predicate valid_or_empty{L}(void *s, size_t n) =
    \object_pointer((char*)s) && \valid(((char*)s)+(0..n-1));

  predicate valid_read_or_empty{L}(void *s, size_t n) =
    \object_pointer((char*)s) && \valid_read(((char*)s)+(0..n-1));
*/
#endif

/*@ requires valid_s1: valid_read_or_empty(s1, n);
  @ requires valid_s2: valid_read_or_empty(s2, n);
  @ requires initialization:s1: \initialized(((char*)s1)+(0..n - 1));
  @ requires initialization:s2: \initialized(((char*)s2)+(0..n - 1));
  @ requires danglingness:s1: non_escaping(s1, n);
  @ requires danglingness:s2: non_escaping(s2, n);
  @ assigns \result \from
  @   indirect:((char*)s1)[0.. n-1], indirect:((char*)s2)[0.. n-1];
  @ ensures logic_spec: \result == memcmp{Pre,Pre}((char*)s1,(char*)s2,n);
  @*/
extern int memcmp (const void *s1, const void *s2, size_t n);

/*@ requires valid:
         valid_read_or_empty(s, n)
         || \valid_read(((unsigned char*)s)+(0..memchr_off((char*)s,c,n)));
  @ requires initialization:
        \initialized(((unsigned char*)s)+(0..n - 1))
     || \initialized(((unsigned char*)s)+(0..memchr_off((char*)s,c,n)));
  @ requires danglingness:
        non_escaping(s, n)
     || non_escaping(s, (size_t)(memchr_off((char*)s,c,n)+1));
  @ assigns \result \from s, c, ((unsigned char*)s)[0..n-1];
  @ behavior found:
  @   assumes char_found: memchr((char*)s,c,n);
  @   ensures result_valid_read: \valid_read((char*)\result);
  @   ensures result_same_base: \base_addr(\result) == \base_addr(s);
  @   ensures result_char: *(char*)\result == c;
  @   ensures result_in_str: \forall integer i;
  @     0 <= i < n ==> *((unsigned char*)s+i) == c
  @     ==> (unsigned char*)\result <= (unsigned char*)s+i;
  @ behavior not_found:
  @   assumes char_not_found: !memchr((char*)s,c,n);
  @   ensures result_null: \result == \null;
  @*/
extern void *memchr(const void *s, int c, size_t n);

// Non-POSIX; GNU extension
// Note: these specifications are simplified w.r.t memchr's
/*@
  requires valid: valid_read_or_empty(s, n);
  requires initialization: \initialized(((unsigned char*)s)+(0 .. n - 1));
  requires danglingness: non_escaping(s, n);
  assigns \result \from s, indirect:c, indirect:((unsigned char*)s)[0..n-1];
  behavior found:
    assumes char_found: memchr((char*)s,c,n);
    ensures result_valid_read: \valid_read((char*)\result);
    ensures result_same_base: \base_addr(\result) == \base_addr(s);
  behavior not_found:
    assumes char_not_found: !memchr((char*)s,c,n);
    ensures result_null: \result == \null;
  complete behaviors;
  disjoint behaviors;
*/
extern void *memrchr(const void *s, int c, size_t n);

// Non-POSIX; GNU extension
// Note: these specifications are simplified w.r.t memchr's
/*@
  requires c_in_s: memchr((char*)s, c, \block_length(s) - 1);
  requires valid:s:
    \let off = \let n = \block_length(s) - 1; memchr_off((char*)s, c, n);
    \valid_read(((char*)s) + (0 .. off));
  requires initialization:s:
    \let off = \let n = \block_length(s) - 1; memchr_off((char*)s, c, n);
    \initialized(((char*)s) + (0 .. off));
  assigns \result \from s, indirect:c, indirect:((unsigned char*)s)[0..];
  ensures result_valid_read: \valid_read((char*)\result);
  ensures result_same_base: \base_addr(\result) == \base_addr(s);
*/
extern void *rawmemchr(const void *s, int c);

// Copy memory

/*@ requires valid_dest: valid_or_empty(dest, n);
  @ requires valid_src: valid_read_or_empty(src, n);
  @ requires separation:
  @   \separated(((char *)dest)+(0..n-1),((char *)src)+(0..n-1));
  @ assigns ((char*)dest)[0..n - 1] \from ((char*)src)[0..n-1];
  @ assigns \result \from dest;
  @ ensures copied_contents: memcmp{Post,Pre}((char*)dest,(char*)src,n) == 0;
  @ ensures result_ptr: \result == dest;
  @*/
extern void *memcpy(void *restrict dest,
		    const void *restrict src, size_t n);

// Non-POSIX; GNU extension
/*@ requires valid_dest: valid_or_empty(dest, n);
  @ requires valid_src: valid_read_or_empty(src, n);
  @ requires separation:
  @   \separated(((char *)dest)+(0..n-1),((char *)src)+(0..n-1));
  @ assigns ((char*)dest)[0..n - 1] \from ((char*)src)[0..n-1];
  @ assigns \result \from dest, n;
  @ ensures copied_contents: memcmp{Post,Pre}((char*)dest,(char*)src,n) == 0;
  @ ensures result_next_byte: \result == dest + n;
  @*/
extern void *mempcpy(void *restrict dest,
                     const void *restrict src, size_t n);

/*@ requires valid_dest: valid_or_empty(dest, n);
  @ requires valid_src: valid_read_or_empty(src, n);
  @ assigns ((char*)dest)[0..n - 1] \from ((char*)src)[0..n-1];
  @ assigns \result \from dest;
  @ ensures copied_contents: memcmp{Post,Pre}((char*)dest,(char*)src,n) == 0;
  @ ensures result_ptr: \result == dest;
  @*/
extern void *memmove(void *dest, const void *src, size_t n);

// Set memory

/*@ requires valid_s: valid_or_empty(s, n);
  @ assigns ((char*)s)[0..n - 1] \from c;
  @ assigns \result \from s;
  @ ensures acsl_c_equiv: memset((char*)s,c,n);
  @ ensures result_ptr: \result == s;
  @*/
extern void *memset(void *s, int c, size_t n);

// Query strings

/*@ requires valid_string_s: valid_read_string(s);
  @ assigns \result \from indirect:s[0..];
  @ ensures acsl_c_equiv: \result == strlen(s);
  @*/
extern size_t strlen (const char *s);

/*@ requires valid_string_s: valid_read_nstring(s, n);
  @ assigns \result \from indirect:s[0..n-1], indirect:n;
  @ ensures result_bounded: \result == strlen(s) || \result == n;
  @*/
extern size_t strnlen (const char *s, size_t n);

/*@ requires valid_string_s1: valid_read_string(s1);
  @ requires valid_string_s2: valid_read_string(s2);
  @ assigns \result \from indirect:s1[0..strlen(s1)],
  @                       indirect:s2[0..strlen(s1)];
  @ ensures acsl_c_equiv: \result == strcmp(s1,s2);
  @*/
extern int strcmp (const char *s1, const char *s2);

/*@ requires valid_string_s1: valid_read_nstring(s1, n); // over-strong
  @ requires valid_string_s2: valid_read_nstring(s2, n); // over-strong
  @ assigns \result \from indirect:s1[0 .. n-1], indirect:s2[0 ..n-1], indirect:n;
  @ ensures acsl_c_equiv: \result == strncmp(s1,s2,n);
  @*/
extern int strncmp (const char *s1, const char *s2, size_t n);

/*@ requires valid_string_s1: valid_read_string(s1); // over-strong
  @ requires valid_string_s2: valid_read_string(s2); // over-strong
  @ assigns \result \from indirect:s1[0..strlen(s1)],
  @                       indirect:s2[0..strlen(s2)]; //missing: \from 'locale'
  @*/
extern int strcoll (const char *s1, const char *s2);

/*@
  requires valid_string_s: valid_read_string(s);
  assigns \result \from s, indirect:s[0..strlen(s)], indirect:c;
  behavior found:
    assumes char_found: strchr(s,c);
    ensures result_valid_string: valid_read_string(\result);
    ensures result_char: *\result == (char)c;
    ensures result_same_base: \base_addr(\result) == \base_addr(s);
    ensures result_in_length: s <= \result <= s + strlen(s);
    ensures result_first_occurrence:
      \forall char* p; s <= p < \result ==> *p != (char)c;
  behavior not_found:
    assumes char_not_found: !strchr(s,c);
    ensures result_null: \result == \null;
  complete behaviors;
  disjoint behaviors;
*/
extern char *strchr(const char *s, int c);

/*@
  requires valid_string_s: valid_read_string(s);
  assigns \result \from s, indirect:s[0..strlen(s)], indirect:c;
  ensures result_valid_string: valid_read_string(\result);
  ensures result_same_base: \base_addr(\result) == \base_addr(s);
*/
extern char *strchrnul(const char *s, int c);

/*@
  requires valid_string_s: valid_read_string(s);
  assigns \result \from s, indirect:s[0 .. strlen(s)], indirect:c;
  ensures result_null_or_same_base:
    \result == \null ||
    (valid_read_string(\result) && \base_addr(\result) == \base_addr(s));
*/
extern char *strrchr(const char *s, int c);

/*@
  requires valid_string_s: valid_read_string(s);
  requires valid_string_reject: valid_read_string(reject);
  assigns \result \from indirect:s[0..strlen(s)],
                        indirect:reject[0..strlen(reject)];
  ensures result_bounded: 0 <= \result <= strlen(s);
*/
extern size_t strcspn(const char *s, const char *reject);

/*@
  requires valid_string_s: valid_read_string(s);
  requires valid_string_accept: valid_read_string(accept);
  assigns \result \from indirect:s[0..strlen(s)],
                        indirect:accept[0..strlen(accept)];
  ensures result_bounded: 0 <= \result <= strlen(s);
*/
extern size_t strspn(const char *s, const char *accept);

/*@
  requires valid_string_s: valid_read_string(s);
  requires valid_string_accept: valid_read_string(accept);
  assigns \result \from s, indirect:s[0..strlen(s)],
                        indirect:accept[0..strlen(accept)];
  ensures result_null_or_same_base:
    \result == \null ||
    (valid_read_string(\result) && \base_addr(\result) == \base_addr(s));
*/
extern char *strpbrk(const char *s, const char *accept);

/*@
  requires valid_string_haystack: valid_read_string(haystack);
  requires valid_string_needle: valid_read_string(needle);
  assigns \result \from haystack, indirect:haystack[0..strlen(haystack)],
                        indirect:needle[0..strlen(needle)];
  ensures result_null_or_in_haystack:
    \result == \null ||
    (valid_read_string(\result) &&
     \base_addr(\result) == \base_addr(haystack) &&
     memcmp{Pre,Pre}(\result,needle,strlen(needle)) == 0);
*/
extern char *strstr(const char *haystack, const char *needle);

/*@
  requires valid_string_haystack: valid_read_string(haystack);
  requires valid_string_needle: valid_read_string(needle);
  assigns \result \from haystack, indirect:haystack[0..strlen(haystack)],
                        indirect:needle[0..strlen(needle)];
  ensures result_null_or_in_haystack:
    \result == \null ||
    (valid_read_string(\result) &&
     \base_addr(\result) == \base_addr(haystack));
  // Note: there is currently no way to concisely write 'memcmp_ignore_case'
*/
extern char *strcasestr(const char *haystack, const char *needle);

// internal state of strtok
//@ ghost __FC_INTERN char *__fc_strtok_ptr;

/*@
  requires valid_string_delim: valid_read_string(delim);
  requires separation: \separated(delim+(0..strlen(delim)), s+(0..strlen(s)));
  assigns s[0..] \from s[0..],
      indirect:s, indirect:__fc_strtok_ptr, indirect:delim[0..strlen(delim)];
  assigns __fc_strtok_ptr[0..] \from __fc_strtok_ptr[0..],
      indirect:s, indirect:__fc_strtok_ptr, indirect:delim[0..strlen(delim)];
  assigns \result \from s, __fc_strtok_ptr, indirect:s[0..],
      indirect:__fc_strtok_ptr[0..], indirect:delim[0..strlen(delim)];
  assigns __fc_strtok_ptr \from \old(__fc_strtok_ptr), s,
                                indirect:__fc_strtok_ptr[0..],
                                indirect:delim[0..strlen(delim)];
  behavior new_str:
    assumes s_not_null: s != \null;
    requires valid_string_s_or_delim_not_found:
      valid_string(s) ||
      (valid_read_string(s) &&
        \forall ℤ i; 0 <= i < strlen(delim) ==> !strchr(s,delim[i]));
    assigns __fc_strtok_ptr
      \from s, indirect:s[0..], indirect:delim[0..strlen(delim)];
    assigns s[0..] \from s[0..], indirect:s, indirect:delim[0..strlen(delim)];
    assigns \result \from s, indirect:s[0..], indirect:delim[0..strlen(delim)];
    ensures result_same_base:
      \result == \null ||
      (valid_read_string(\result) && \base_addr(\result) == \base_addr(s));
    ensures ptr_valid_string: valid_read_string(__fc_strtok_ptr);
    ensures ptr_same_base: \base_addr(__fc_strtok_ptr) == \base_addr(s);
  behavior resume_str:
    assumes s_null: s == \null;
    requires not_first_call: __fc_strtok_ptr != \null;
    assigns __fc_strtok_ptr[0..] \from __fc_strtok_ptr[0..],
                                       indirect:__fc_strtok_ptr,
                                       indirect:delim[0..strlen(delim)];
    assigns __fc_strtok_ptr \from \old(__fc_strtok_ptr),
                                  indirect:__fc_strtok_ptr[0..],
                                  indirect:delim[0..strlen(delim)];
    assigns \result \from __fc_strtok_ptr, indirect:__fc_strtok_ptr[0..],
                          indirect:delim[0..strlen(delim)];
    ensures result_same_base:
      \result == \null ||
      (valid_read_string(\result) &&
        \base_addr(\result) == \base_addr(\old(__fc_strtok_ptr)));
    ensures ptr_valid_string: valid_read_string(__fc_strtok_ptr);
    ensures ptr_same_base:
      \base_addr(__fc_strtok_ptr) == \base_addr(\old(__fc_strtok_ptr));
  complete behaviors;
  disjoint behaviors;
*/
extern char *strtok(char *restrict s, const char *restrict delim);

/*@
  requires valid_string_delim: valid_read_string(delim);
  requires separation: \separated(delim+(0..strlen(delim)), s+(0..strlen(s)),
                                  *saveptr+(0..));
  requires valid_saveptr: \valid(saveptr);
  assigns s[0..] \from s[0..],
      indirect:s, indirect:*saveptr, indirect:delim[0..strlen(delim)];
  assigns (*saveptr)[0..] \from (*saveptr)[0..],
      indirect:s, indirect:*saveptr, indirect:delim[0..strlen(delim)];
  assigns \result \from s, *saveptr, indirect:s[0..],
      indirect:(*saveptr)[0..], indirect:delim[0..strlen(delim)];
  assigns *saveptr \from \old(*saveptr), s,
                         indirect:(*saveptr)[0..],
                         indirect:delim[0..strlen(delim)];
  behavior new_str:
    assumes s_not_null: s != \null;
    requires valid_string_s_or_delim_not_found:
      valid_string(s) ||
      (valid_read_string(s) &&
        \forall ℤ i; 0 <= i < strlen(delim) ==> !strchr(s,delim[i]));
    assigns *saveptr
      \from s, indirect:s[0..], indirect:delim[0..strlen(delim)];
    assigns s[0..] \from s[0..], indirect:s, indirect:delim[0..strlen(delim)];
    assigns \result \from s, indirect:s[0..], indirect:delim[0..strlen(delim)];
    ensures result_same_base:
      \result == \null ||
      (valid_read_string(\result) && \base_addr(\result) == \base_addr(s));
    ensures saveptr_valid_string: valid_read_string(*saveptr);
    ensures saveptr_same_base: \base_addr(*saveptr) == \base_addr(s);
  behavior resume_str:
    assumes s_null: s == \null;
    requires not_first_call: *saveptr != \null;
    requires initialization:saveptr: \initialized(saveptr);
    assigns (*saveptr)[0..] \from (*saveptr)[0..],
                                       indirect:*saveptr,
                                       indirect:delim[0..strlen(delim)];
    assigns *saveptr \from \old(*saveptr),
                           indirect:(*saveptr)[0..],
                           indirect:delim[0..strlen(delim)];
    assigns \result \from *saveptr, indirect:(*saveptr)[0..],
                          indirect:delim[0..strlen(delim)];
    ensures result_same_base:
      \result == \null ||
      (valid_read_string(\result) &&
        \base_addr(\result) == \base_addr(\old(*saveptr)));
    ensures saveptr_valid_string: valid_read_string(*saveptr);
    ensures saveptr_same_base:
      \base_addr(*saveptr) == \base_addr(\old(*saveptr));
  complete behaviors;
  disjoint behaviors;
*/
extern char *strtok_r(char *restrict s, const char *restrict delim, char **restrict saveptr);

/*@
  requires valid_stringp: \valid(stringp);
  assigns *stringp[0..] \from indirect:delim[0..strlen(delim)], *stringp[0..];
  assigns \result \from *stringp;
  behavior no_stringp:
    assumes stringp_null: *stringp == \null;
    ensures result_null: \result == \null;
  behavior valid_stringp:
    assumes stringp_not_null: *stringp != \null;
    requires valid_string_stringp: valid_string(*stringp);
    ensures valid_result: valid_string(\result) &&
                          \base_addr(\result) == \base_addr(*stringp);
*/
extern char *strsep (char **stringp, const char *delim);

__FC_EXTERN char __fc_strerror[64];

// Note: postcondition "result_nul_terminated" is only a temporary patch,
//       to help plug-ins which are currently unable to reduce the post-state
//       using only 'result_valid_string'.
/*@ assigns \result \from &__fc_strerror, indirect:errnum;
  @ ensures result_internal_str: \result == &__fc_strerror[0];
  @ ensures result_nul_terminated: \result[63] == 0;
  @ ensures result_valid_string: valid_read_string(\result);
  @*/
extern char *strerror(int errnum);

// Copy strings

/*@ requires valid_string_src: valid_read_string(src);
  @ requires room_string: \valid(dest+(0..strlen(src)));
  @ requires separation:
  @   \separated(dest+(0..strlen(src)), src+(0..strlen(src)));
  @ assigns dest[0..strlen(src)] \from src[0..strlen(src)];
  @ assigns \result \from dest;
  @ ensures equal_contents: strcmp(dest,src) == 0;
  @ ensures result_ptr: \result == dest;
  @*/
extern char *strcpy(char *restrict dest, const char *restrict src);

/*@
  @ requires valid_nstring_src: valid_read_nstring(src, n);
  @ requires room_nstring: \valid(dest+(0 .. n-1));
  @ requires separation:
  @   \separated(dest+(0..n-1), src+(0..n-1));
  @ assigns dest[0..n - 1] \from src[0..n-1];
  @ assigns \result \from dest;
  @ ensures result_ptr: \result == dest;
  @ ensures initialization: \initialized(dest+(0 .. n-1));
  @ behavior complete:
  @   assumes src_fits: strlen(src) < n;
  @   ensures equal_after_copy: strcmp(dest,src) == 0;
  @ behavior partial:
  @   assumes src_too_long: n <= strlen(src);
  @   ensures equal_prefix: memcmp{Post,Post}(dest,src,n) == 0;
  @*/
extern char *strncpy(char *restrict dest,
		     const char *restrict src, size_t n);

/*@ // Non-POSIX, but often present; note that, unlike strncpy, this has no
  @ // "official specification" other than manpages. They state that src is
  @ // a *string*, therefore its precondition is stricter than that of strncpy.
  @ requires valid_string_src: valid_read_string(src);
  @ requires room_nstring: \valid(dest+(0..n-1));
  @ requires separation:
  @   \separated(dest+(0..n-1), src+(0..\max(n-1,strlen(src))));
  @ assigns dest[0..n-1] \from src[0..n-1];
  @ assigns \result \from indirect:src, indirect:src[0..n-1], indirect:n;
  @ ensures initialization: \initialized(dest+(0..\min(strlen(src),n-1)));
  @ ensures bounded_result: \result == strlen(src);
 */
size_t strlcpy(char * restrict dest, const char * restrict src, size_t n);

// stpcpy is POSIX.1-2008
/*@ requires valid_string_src: valid_read_string(src);
  @ requires room_string: \valid(dest+(0..strlen(src)));
  @ requires separation:
  @   \separated(dest+(0..strlen(src)), src+(0..strlen(src)));
  @ assigns dest[0..strlen(src)] \from src[0..strlen(src)];
  @ assigns \result \from dest;
  @ ensures equal_contents: strcmp(dest,src) == 0;
  @ ensures points_to_end: \result == dest + strlen(dest);
  @*/
extern char *stpcpy(char *restrict dest, const char *restrict src);

/*@
  @ requires valid_string_src: valid_read_string(src);
  @ requires valid_string_dest: valid_string(dest);
  @ requires room_string: \valid(dest+(0..strlen(dest) + strlen(src)));
  @ requires separation:
  @   \separated(dest+(0..strlen(dest)+strlen(src)), src+(0..strlen(src)));
  @ assigns dest[strlen(dest)..strlen(dest) + strlen(src)]
  @   \from src[0..strlen(src)];
  @ ensures sum_of_lengths: strlen(dest) == \old(strlen(dest) + strlen(src));
  @ assigns \result \from dest;
  @ ensures initialization:dest:
  @   \initialized(dest+(0..\old(strlen(dest) + strlen(src))));
  @ ensures dest_null_terminated: dest[\old(strlen(dest) + strlen(src))] == 0;
  @ ensures result_ptr: \result == dest;
  @*/
extern char *strcat(char *restrict dest, const char *restrict src);

// ISO C23, 7.26.3.2, footnote 378:
// "A terminating null character is always appended to the result.
// Thus, the maximum number of characters that can end up in the array pointed
// to by s1 is strlen(s1)+n+1"
/*@
  requires valid_nstring_src: valid_read_nstring(src, n);
  requires valid_string_dest: valid_string(dest);
  requires separation:
    \separated(dest+(0..strlen(dest)+strnlen(src, n)),
               src+(0..strnlen(src, n)));
  assigns dest[strlen(dest) .. strlen(dest) + n] \from src[0 .. n-1];
  assigns \result \from dest;
  ensures result_ptr: \result == dest;
  behavior complete:
    assumes src_fits: strnlen(src, n) < n; // strnlen(src, n) == strlen(src)
    requires room_string: \valid(dest + strlen(dest) + (0 .. strlen(src)));
    assigns dest[strlen(dest) .. strlen(dest) + strlen(src)]
      \from src[0 .. strlen(src)-1];
    assigns \result \from dest;
    ensures sum_of_lengths: strlen(dest) == \old(strlen(dest) + strlen(src));
  behavior partial:
    assumes src_too_large: strnlen(src, n) == n;
    requires room_string: \valid(dest + strlen(dest) + (0 .. n));
    assigns dest[strlen(dest) .. strlen(dest) + n]
      \from src[0 .. n - 1];
    assigns \result \from dest;
    ensures sum_of_bounded_lengths: strlen(dest) == \old(strlen(dest)) + n;
  complete behaviors;
  disjoint behaviors;
*/
extern char *strncat(char *restrict dest, const char *restrict src, size_t n);

/*@ // Non-POSIX, but often present
  @
  @ requires valid_string_src: valid_read_string(src);
  @ requires valid_string_dest: valid_string(dest);
  @ requires room_nstring: \valid(dest+(0..n-1));
  @ requires separation:
  @   \separated(dest+(0..\min(n-1, strlen(dest)+strlen(src))),
  @              src+(0..strnlen(src, n-1)));
  @ assigns dest[strlen(dest)..n] \from indirect:n, src[0..strlen(src)];
  @ assigns \result \from indirect:src, indirect:src[0..n-1], indirect:n;
  @ ensures bounded_result: \result == strlen(dest) + strlen(src);
  @*/
extern size_t strlcat(char *restrict dest, const char *restrict src, size_t n);

/*@ // missing: assigns ... \from 'locale';
  requires valid_dest: \valid(dest+(0..n - 1));
  requires valid_string_src: valid_read_string(src);
  requires separation: \separated(dest+(0..n - 1), src+(0..strlen(src)));
  assigns dest[0..n - 1] \from src[0..strlen(src)], indirect:n;
  assigns \result \from indirect:src[0..strlen(src)]; // can be greater than n
*/
extern size_t strxfrm(char *restrict dest, const char *restrict src, size_t n);

// Non-POSIX; GNU extension
/*@
  requires valid_haystack: \valid_read((char*)haystack + (0 .. haystacklen-1));
  requires valid_needle: \valid_read((char*)needle + (0 .. needlelen-1));
  assigns \result \from haystack,
                        indirect:((char*)haystack)[0 .. haystacklen-1],
                        indirect:((char*)needle)[0 .. needlelen-1];
  ensures result_null_or_valid:
    \result == \null || \valid_read((char*)\result);
  ensures result_null_or_same_base:
    \result == \null || \base_addr(\result) == \base_addr(haystack);
*/
extern void *memmem(const void *haystack, size_t haystacklen,
             const void *needle, size_t needlelen);

// Allocate strings

/*@
  requires valid_string_s: valid_read_string(s);
  allocates \result;
  assigns \result \from indirect:s[0..strlen(s)], indirect:__fc_heap_status;
  behavior allocation:
    assumes can_allocate: is_allocable(strlen(s)+1);
    assigns __fc_heap_status \from __fc_heap_status,
                                   indirect:s[0 .. strlen(s)];
    assigns \result \from indirect:s[0..strlen(s)], indirect:__fc_heap_status;
    ensures allocation: \fresh(\result,strlen(s)+1);
    ensures result_valid_string_and_same_contents:
      valid_string(\result) && strcmp(\result,s) == 0;
  behavior no_allocation:
    assumes cannot_allocate: !is_allocable(strlen(s)+1);
    allocates \nothing;
    assigns \result \from \nothing;
    ensures result_null: \result == \null;
*/
extern char *strdup (const char *s);

// From the strndup(3p) POSIX man page:
// ... strndup() copies at most size plus one bytes into the newly allocated
// memory, terminating the new string with a NUL character.
/*@
  requires valid_string_s: valid_read_string(s);
  allocates \result;
  assigns \result \from indirect:s[0..strlen(s)], indirect:n,
                        indirect:__fc_heap_status;
  behavior allocation:
    assumes can_allocate: is_allocable(strnlen(s, n) + 1);
    assigns __fc_heap_status \from indirect:s, indirect:n, __fc_heap_status;
    assigns \result \from indirect:s[0 .. strlen(s)], indirect:n,
                          indirect:__fc_heap_status;
    ensures allocation: \fresh(\result, strnlen(s, n) + 1);
    ensures result_valid_string_bounded_and_same_prefix:
      \valid(\result+(0 .. strnlen(s,n))) &&
      valid_string(\result) && strlen(\result) <= n &&
      strncmp(\result,s,n) == 0;
  behavior no_allocation:
    assumes cannot_allocate: !is_allocable(strnlen(s, n) + 1);
    allocates \nothing;
    assigns \result \from \nothing;
    ensures result_null: \result == \null;
*/
extern char *strndup (const char *s, size_t n);

// More POSIX, non-C99 functions
/*@
  requires valid_string_src: valid_read_nstring(src, n);
  requires valid_dest: \valid(dest+(0 .. n-1));
  requires separation: \separated(src+(0 .. n-1), dest+(0 .. n-1));
  assigns dest[0 .. n-1] \from src[0 .. n-1], indirect:n;
  assigns \result \from dest, indirect:src[0 .. n-1], indirect:n;
  ensures initialization: \initialized(dest+(0 .. n-1));
  behavior src_is_a_string:
    assumes src_has_nul: strlen(src) < n;
    ensures valid_string_dest: valid_read_string(dest);
    ensures same_contents: strcmp(dest, src) == 0;
    ensures dest_nul_padded:
      \forall integer i; strlen(src) <= i < n ==> dest[i] == 0;
    ensures result_points_to_nul: \result == dest + strlen(src);
  behavior src_too_long:
    assumes src_no_nul: \forall integer i; 0 <= i < n ==> src[i] != 0;
    ensures same_partial_contents: strncmp(src, dest, n) == 0;
    ensures result_points_to_end: \result == dest + n;
*/
extern char *stpncpy(char *restrict dest, const char *restrict src, size_t n);

//extern int strcoll_l(const char *s1, const char *s2, locale_t locale);

//extern char *strerror_l(int errnum, locale_t locale);

/*@
  assigns \result \from indirect:errnum, indirect:buflen;
  assigns strerrbuf[0 .. buflen-1] \from indirect:errnum, indirect:buflen;
  //missing: assigns errno (EINVAL, ERANGE)
*/
extern int strerror_r(int errnum, char *strerrbuf, size_t buflen);

__FC_EXTERN char __fc_strsignal[64];

/*@ //missing: requires valid_signal(signum); assigns \from 'locale';
  @ assigns \result \from &__fc_strsignal, indirect:signum;
  @ ensures result_internal_str: \result == &__fc_strsignal[0];
  @ ensures result_nul_terminated: \result[63] == 0;
  @ ensures result_valid_string: valid_read_string(\result);
  @*/
extern char *strsignal(int signum);

//extern size_t strxfrm_l(char *restrict s1, const char *restrict s2, size_t n,
//                        locale_t locale);

__END_DECLS

/* Include strings.h: this is what BSD does, and glibc does something
   equivalent (having copied prototypes to string.h). */
#include <strings.h>

__POP_FC_STDLIB
#endif /* _STRING_H_ */

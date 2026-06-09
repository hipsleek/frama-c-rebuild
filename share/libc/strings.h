/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_STRINGS_H
#define __FC_STRINGS_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_size_t.h"
#include "__fc_string_axiomatic.h"

__BEGIN_DECLS

/*@
  assigns \result \from indirect:((char*)s1)[0 .. n-1],
                        indirect:((char*)s2)[0 .. n-1];
*/
extern int bcmp(const void *s1, const void *s2, size_t n);

/*@
  assigns ((char*)dest)[0 .. n-1] \from ((char*)src)[0 .. n-1], indirect:n;
*/
extern void bcopy(const void *src, void *dest, size_t n);


/*@ requires valid_memory_area: \valid (((char*) s)+(0 .. n-1));
  assigns ((char*) s)[0 .. n-1] \from \nothing;
  ensures s_initialized:initialization:\initialized(((char*) s)+(0 .. n-1));
  ensures zero_initialized: \subset(((char*) s)[0 .. n-1], {0}); */
extern void bzero(void *s, size_t n);

/*@
  assigns \result \from i;
*/
extern int ffs(int i);

#if _POSIX_C_SOURCE - 0 < 200809L
// index and rindex were removed in POSIX-1.2008

/*@
  assigns \result \from s, indirect:s[0 .. strlen(s)], indirect:c;
*/
extern char *index(const char *s, int c);

/*@
  assigns \result \from s, indirect:s[0 .. strlen(s)], indirect:c;
*/
extern char *rindex(const char *s, int c);
#endif

/*@
  requires valid_string_s1: valid_read_string(s1);
  requires valid_string_s2: valid_read_string(s2);
  assigns \result \from indirect:s1[0..], indirect:s2[0..];
*/
extern int strcasecmp(const char *s1, const char *s2);

/*@
  requires valid_string_s1: valid_read_nstring(s1, n);
  requires valid_string_s2: valid_read_nstring(s2, n);
  assigns \result \from indirect:n, indirect:s1[0..n-1], indirect:s2[0..n-1];
*/
extern int strncasecmp(const char *s1, const char *s2, size_t n);

__END_DECLS

__POP_FC_STDLIB
#endif

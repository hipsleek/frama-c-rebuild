/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*  Free Software Foundation, Inc.                                        */
/*                                                                        */
/**************************************************************************/

// Non-POSIX; this file is defined in the GNU libc.

#ifndef __FC_ARGZ_H
#define __FC_ARGZ_H

#include "features.h"
__PUSH_FC_STDLIB

#include <errno.h>
#include <stdlib.h>
#include <string.h>

__BEGIN_DECLS

typedef int error_t;

/*@
  allocates *argz;
  assigns (*argz)[0..], *len, \result \from argv[0..][0..];
*/
extern error_t argz_create (char *const argv[], char **restrict argz,
                            size_t *restrict len);

/*@
  allocates *argz;
  assigns (*argz)[0..], *len, \result \from string[0..], sep, (*argz)[0..], *len;
*/
extern error_t argz_create_sep (const char *restrict string,
                                int sep, char **restrict argz,
                                size_t *restrict len);

/*@ assigns \result \from indirect:argz[0 .. strlen(argz)], indirect:len; */
extern size_t argz_count (const char *argz, size_t len);

/*@ assigns argv[0..][0..] \from argz[0 .. len], indirect:len; */
extern void argz_extract (const char *restrict argz, size_t len,
                          char **restrict argv);

/*@ assigns argz[0..] \from argz[0..], indirect:len, sep; */
extern void argz_stringify (char *argz, size_t len, int sep);

/*@
  allocates *argz;
  assigns (*argz)[0..], *argz_len \from (*argz)[0..], *argz_len,
                                        buf[0..], buf_len;
  assigns \result \from indirect:(*argz)[0..], indirect:*argz_len,
                        indirect:buf[0..], indirect:buf_len;
*/
extern error_t argz_append (char **restrict argz,
                            size_t *restrict argz_len,
                            const char *restrict buf, size_t buf_len);

/*@
  allocates *argz;
  assigns (*argz)[0..], *argz_len, \result \from (*argz)[0..], *argz_len,
                                                 str[0..];
*/
extern error_t argz_add (char **restrict argz,
                         size_t *restrict argz_len,
                         const char *restrict str);

/*@
  allocates *argz;
  assigns (*argz)[0..], *argz_len, \result \from string[0..], delim;
*/
extern error_t argz_add_sep (char **restrict argz,
                             size_t *restrict argz_len,
                             const char *restrict string, int delim);

/*@
  frees *argz;
  assigns (*argz)[0..], *argz_len \from (*argz)[0..], *argz_len, entry[0..];
*/
extern void argz_delete (char **restrict argz,
                         size_t *restrict argz_len,
                         char *restrict entry);

/*@
  allocates *argz;
  assigns (*argz)[0..], *argz_len, \result \from (*argz)[0..], *argz_len,
                                                 before, before[0..],
                                                 entry[0..];
*/
extern error_t argz_insert (char **restrict argz,
                            size_t *restrict argz_len,
                            char *restrict before,
                            const char *restrict entry);

/*@
  allocates *argz;
  assigns (*argz)[0..], *argz_len, *replace_count, \result
  \from
      (*argz)[0..], *argz_len, str[0..], with[0..], *replace_count;
*/
extern error_t argz_replace (char **restrict argz,
                             size_t *restrict argz_len,
                             const char *restrict str,
                             const char *restrict with,
                             unsigned int *restrict replace_count);

/*@
  assigns \result \from argz[0..], argz_len, entry;
*/
extern char *argz_next (const char *restrict argz, size_t argz_len,
                        const char *restrict entry);

__END_DECLS

__POP_FC_STDLIB
#endif

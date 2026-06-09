/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_WORDEXP_H
#define __FC_WORDEXP_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_size_t.h"

__BEGIN_DECLS

typedef struct __fc_wordexp_t {
  size_t we_wordc;
  char **we_wordv;
  size_t we_offs;
} wordexp_t;

#define WRDE_DOOFFS (1 << 0)
#define WRDE_APPEND (1 << 1)
#define WRDE_NOCMD (1 << 2)
#define WRDE_REUSE (1 << 3)
#define WRDE_SHOWERR (1 << 4)
#define WRDE_UNDEF (1 << 5)

#define WRDE_NOSPACE 1
#define WRDE_BADCHAR 2
#define WRDE_BADVAL 3
#define WRDE_CMDSUB 4
#define WRDE_SYNTAX 5

/*@
  allocates pwordexp->we_wordv;
  //missing: assigns from 'filesystem', 'environment'
  assigns \result \from indirect:words[0..], indirect:pwordexp, indirect:flags;
  assigns pwordexp->we_wordc, pwordexp->we_wordv[0..][0..] \from words[0..],
                                                                 indirect:flags;
 */
extern int wordexp(const char *restrict words, wordexp_t *restrict pwordexp,
                   int flags);

/*@
  frees pwordexp->we_wordv[0..], pwordexp->we_wordv;
  assigns *pwordexp \from *pwordexp;
 */
extern void wordfree(wordexp_t *pwordexp);

__END_DECLS

__POP_FC_STDLIB
#endif

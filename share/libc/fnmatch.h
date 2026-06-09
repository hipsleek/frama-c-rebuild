/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_FNMATCH_H
#define __FC_FNMATCH_H
#include "features.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

// The values for the constants below are based on those
// of the glibc, declared in the order given by POSIX.1-2008.

#define FNM_NOMATCH 1
#define FNM_PATHNAME (1 << 0)
#define FNM_PERIOD (1 << 2)
#define FNM_NOESCAPE (1 << 1)

/*@
  assigns \result \from indirect:pattern[0..], indirect:string[0..],
    indirect:flags; //missing: from 'filesystem'
*/
extern int fnmatch(const char *pattern, const char *string, int flags);

__END_DECLS
__POP_FC_STDLIB
#endif

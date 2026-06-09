/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_GLOB_H
#define __FC_GLOB_H
#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_machdep.h"
#include "__fc_string_axiomatic.h"

#define	GLOB_ERR	(1 << 0)/* Return on read errors.  */
#define	GLOB_MARK	(1 << 1)/* Append a slash to each name.  */
#define	GLOB_NOSORT	(1 << 2)/* Don't sort the names.  */
#define	GLOB_DOOFFS	(1 << 3)/* Insert PGLOB->gl_offs NULLs.  */
#define	GLOB_NOCHECK	(1 << 4)/* If nothing matches, return the pattern.  */
#define	GLOB_APPEND	(1 << 5)/* Append to results of a previous call.  */
#define	GLOB_NOESCAPE	(1 << 6)/* Backslashes don't quote metacharacters.  */
#define	GLOB_PERIOD	(1 << 7)/* Leading `.' can be matched by metachars.  */

// Non-POSIX
#define GLOB_MAGCHAR (1 << 8)
#define GLOB_ALTDIRFUNC (1 << 9)
#define GLOB_BRACE (1 << 10)
#define GLOB_NOMAGIC (1 << 11)
#define GLOB_TILDE (1 << 12)
#define GLOB_ONLYDIR (1 << 13)
#define GLOB_TILDE_CHECK (1 << 14)


#define	GLOB_NOSPACE	1	/* Ran out of memory.  */
#define	GLOB_ABORTED	2	/* Read error.  */
#define	GLOB_NOMATCH	3	/* No matches found.  */
#define GLOB_NOSYS	4	/* Not implemented.  */

__BEGIN_DECLS

typedef struct __fc_glob_t {
  __SIZE_T gl_pathc;		/* Count of paths matched by the pattern.  */
  char **gl_pathv;		/* List of matched pathnames.  */
  __SIZE_T gl_offs;		/* Slots to reserve in `gl_pathv'.  */
  int gl_flags;		/* Set to FLAGS, maybe | GLOB_MAGCHAR.  */
  
  /* If the GLOB_ALTDIRFUNC flag is set, the following functions
     are used instead of the normal file access functions.  */
  void (*gl_closedir) (void *);
#ifdef __USE_GNU
  struct dirent *(*gl_readdir) (void *);
#else
  void *(*gl_readdir) (void *);
#endif
  void *(*gl_opendir) (__const char *);
#ifdef __USE_GNU
  int (*gl_lstat) (__const char *__restrict, struct stat *__restrict);
  int (*gl_stat) (__const char *__restrict, struct stat *__restrict);
#else
  int (*gl_lstat) (__const char *__restrict, void *__restrict);
  int (*gl_stat) (__const char *__restrict, void *__restrict);
#endif
} glob_t;

/*@
  allocates pglob->gl_pathv;
  assigns *pglob \from indirect:pattern[0 .. strlen(pattern)],
                       flags,
                       indirect:errfunc;
  assigns \result \from indirect:pattern[0 .. strlen(pattern)],
                        indirect:flags,
                        indirect:errfunc;
*/
extern int glob(const char *pattern, int flags,
                int (*errfunc) (const char *epath, int eerrno),
                glob_t *pglob);

/*@
  frees pglob->gl_pathv;
  assigns *pglob \from *pglob;
 */
extern void globfree(glob_t *pglob);

__END_DECLS

__POP_FC_STDLIB
#endif

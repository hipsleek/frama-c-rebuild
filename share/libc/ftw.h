/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_FTW_H
#define __FC_FTW_H
#include "features.h"
__PUSH_FC_STDLIB
#include <errno.h>
// From POSIX 1.2008: "Inclusion of the <ftw.h> header may also make visible
//                     all symbols from <sys/stat.h>".
#include <sys/stat.h>

__BEGIN_DECLS

struct FTW
{
  int base;
  int level;
};

enum __fc_ftw
{
  FTW_F,
#define FTW_F FTW_F
  FTW_D,
#define FTW_D FTW_D
  FTW_DNR,
#define FTW_DNR FTW_DNR
  FTW_DP,
#define FTW_DP FTW_DP
  FTW_NS,
#define FTW_NS FTW_NS
  FTW_SL,
#define FTW_SL FTW_SL
  FTW_SLN,
#define FTW_SLN FTW_SLN
};

enum __fc_nftw
{
  NFTW_PHYS,
#define NFTW_PHYS NFTW_PHYS
  NFTW_MOUNT,
#define NFTW_MOUNT NFTW_MOUNT
  NFTW_DEPTH,
#define NFTW_DEPTH NFTW_DEPTH
  NFTW_CHDIR,
#define NFTW_CHDIR NFTW_CHDIR
};

/*@
  // missing: assigns 'filesystem', \from 'filesystem', and also everything
  //          that fn can assign to.
  assigns \result, errno \from indirect:path[0..], indirect:fn, indirect:ndirs;
 */
int ftw(const char *path,
        int (*fn)(const char *, const struct stat *ptr, int flag), int ndirs);

/*@
  // missing: assigns 'filesystem', \from 'filesystem', and also everything
  //          that fn can assign to.
  assigns \result, errno \from indirect:path[0..], indirect:fn,
                               indirect:fd_limit, indirect:flags;
 */
int nftw(const char *path,
         int (*fn)(const char *, const struct stat *, int, struct FTW *),
         int fd_limit, int flags);

__END_DECLS

__POP_FC_STDLIB
#endif

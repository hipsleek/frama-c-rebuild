/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_AT_H
#define __FC_DEFINE_AT_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#define AT_FDCWD -100
#define AT_EACCESS 0x200
#define AT_SYMLINK_NOFOLLOW 0x100
#define AT_SYMLINK_FOLLOW 0x400
#define AT_REMOVEDIR 0x200

// Non-POSIX (GNU extensions)
#define AT_EMPTY_PATH 0x1000
#define AT_RECURSIVE 0x8000
#define AT_STATX_DONT_SYNC 0x4000
#define AT_STATX_FORCE_SYNC 0x2000
#define AT_STATX_SYNC_AS_STAT 0x0000
#define AT_STATX_SYNC_TYPE 0x6000

__END_DECLS

__POP_FC_STDLIB
#endif // __FC_DEFINE_AT

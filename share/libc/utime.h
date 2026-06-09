/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_UTIME_H
#define __FC_UTIME_H
#include "features.h"
__PUSH_FC_STDLIB
#include <sys/time.h>
#include <errno.h>

__BEGIN_DECLS

struct utimbuf {
  time_t actime;  /* access time */
  time_t modtime; /* modification time */
};

/*@
  assigns \result, errno \from indirect:filename[0..], indirect:*times;
  //missing: assigns 'filesystem', \from *times, \from 'current time';
 */
extern int utime(const char *filename, const struct utimbuf *times);

__END_DECLS
__POP_FC_STDLIB
#endif

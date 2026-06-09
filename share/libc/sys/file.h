/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_FILE_H
#define __FC_SYS_FILE_H

#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

// Note: this file is not C11 nor POSIX, but Linux-specific.
// The values for the constants below are based on the glibc.

#define L_SET 0
#define L_INCR 1
#define L_XTND 2

#define LOCK_SH 1
#define LOCK_EX 2
#define LOCK_UN 8

#define LOCK_NB 4

/*@ // missing: may assign errno to EBADF, EINTR, EINVAL, ENOLCK, EWOULDBLOCK
    // missing: assigns \result, 'filesystem' \from 'filesystem'
  assigns \result \from indirect:fd, indirect:operation;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int flock(int fd, int operation);

__END_DECLS
__POP_FC_STDLIB
#endif

/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_IPC_H
#define __FC_SYS_IPC_H
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "../__fc_define_mode_t.h"
#include "../__fc_define_uid_and_gid.h"
#include "../__fc_define_key_t.h"

struct ipc_perm {
  uid_t uid;
  gid_t gid;
  uid_t cuid;
  gid_t cgid;
  mode_t mode;
};

// The values for the constants below are based on an x86 Linux,
// declared in the order given by POSIX.1-2008.

#define IPC_CREAT 01000
#define IPC_EXCL 02000
#define IPC_NOWAIT 04000

#define IPC_PRIVATE ((key_t) 0)

#define IPC_RMID 0
#define IPC_SET 1
#define IPC_STAT 2

/*@
 assigns \result \from path[0..], id;
*/
extern key_t ftok(const char *path, int id);

__END_DECLS
__POP_FC_STDLIB
#endif

/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_SHM_H
#define __FC_SYS_SHM_H
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "../__fc_define_pid_t.h"
#include "../__fc_define_size_t.h"
#include "../__fc_define_time_t.h"

// POSIX: "the <sys/shm.h> header shall include the <sys/ipc.h> header."
#include <sys/ipc.h>

// The values for the constants below are based on an x86 Linux,
// declared in the order given by POSIX.1-2008.

#define SHM_RDONLY 010000
#define SHM_RND 020000

// TODO: parametrize the page size according to the machdep?
#define __FC_PAGE_SIZE 4096
#define SHMLBA __FC_PAGE_SIZE

typedef unsigned long shmatt_t;

struct shmid_ds {
  struct ipc_perm shm_perm;
  size_t shm_segsz;
  pid_t shm_lpid;
  pid_t shm_cpid;
  shmatt_t shm_nattch;
  time_t shm_atime;
  time_t shm_dtime;
  time_t shm_ctime;
};

/*@
  allocates \result;
  assigns \result \from indirect:shmid, shmaddr, indirect:shmflg;
*/
extern void *shmat(int shmid, const void *shmaddr, int shmflg);

/*@
  assigns \result, *buf \from shmid, cmd;
*/
extern int shmctl(int shmid, int cmd, struct shmid_ds *buf);

/*@
  frees shmaddr;
  assigns \result \from shmaddr;
*/
extern int shmdt(const void *shmaddr);

/*@
  assigns \result \from key, size, shmflg;
*/
extern int shmget(key_t key, size_t size, int shmflg);

__END_DECLS
__POP_FC_STDLIB
#endif

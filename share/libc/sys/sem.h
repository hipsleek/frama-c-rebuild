/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_SEM_H
#define __FC_SYS_SEM_H
#include "../features.h"
__PUSH_FC_STDLIB
#include "../__fc_define_pid_t.h"
#include "../__fc_define_size_t.h"
#include "../__fc_define_time_t.h"
#include <sys/ipc.h>

__BEGIN_DECLS

#define SEM_UNDO 0x1000

#define GETNCNT 14
#define GETPID  11
#define GETVAL  12
#define GETALL  13
#define GETZCNT 15
#define SETVAL  16
#define SETALL  17

struct semid_ds {
  struct ipc_perm sem_perm;
  unsigned short sem_nsems;
  time_t sem_otime;
  time_t sem_ctime;
};

// POSIX 2018 states: "a semaphore shall be represented by an anonymous
// structure, which shall include the following members".
struct __fc_sem {
  unsigned short semval;
  pid_t sempid;
  unsigned short semncnt;
  unsigned short semzcnt;
};

struct sembuf {
  unsigned short sem_num;
  short sem_op;
  short sem_flg;
};

/*@
  assigns \result \from semid, semnum, cmd;
  //missing: assigns <optional 4th argument>
*/
extern int semctl(int semid, int semnum, int cmd, ...);

/*@
  assigns \result \from key, nsems, semflg;
*/
extern int semget(key_t key, int nsems, int semflg);

/*@
  assigns \result \from semid, *sops, nsops;
*/
extern int semop(int semid, struct sembuf *sops, size_t nsops);

__END_DECLS

__POP_FC_STDLIB
#endif

/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_MSG_H
#define __FC_SYS_MSG_H
#include "../features.h"
__PUSH_FC_STDLIB
#include "__fc_define_pid_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_ssize_t.h"
#include "__fc_define_time_t.h"
#include <sys/ipc.h>

__BEGIN_DECLS

typedef unsigned long msgqnum_t;
typedef unsigned long msglen_t;

#define MSG_NOERROR 010000

struct msqid_ds {
 struct ipc_perm msg_perm;
 msgqnum_t msg_qnum;
 msglen_t msg_qbytes;
 pid_t msg_lspid;
 pid_t msg_lrpid;
 time_t msg_stime;
 time_t msg_rtime;
 time_t msg_ctime;
};

/*@
  assigns \result, *buf \from msqid, cmd;
*/
extern int msgctl(int msqid, int cmd, struct msqid_ds *buf);

/*@
  assigns \result \from key, msgflg;
*/
extern int msgget(key_t key, int msgflg);

/*@
  assigns \result, ((char*)msgp)[0 .. msgsz-1] \from msqid, msgsz, msgtyp,
    msgflg;
*/
extern ssize_t msgrcv(int msqid, void *msgp, size_t msgsz, long msgtyp,
                      int msgflg);

/*@
  assigns \result \from msqid, ((char*)msgp)[0 .. msgsz-1], msgsz, msgflg;
*/
extern int msgsnd(int msqid, const void *msgp, size_t msgsz, int msgflg);

__END_DECLS

__POP_FC_STDLIB
#endif

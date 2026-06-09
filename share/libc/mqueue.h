/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_MQUEUE_H
#define __FC_MQUEUE_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_size_t.h"
#include "__fc_define_ssize_t.h"
#include <errno.h>
#include <signal.h>

__BEGIN_DECLS

typedef int mqd_t;

struct mq_attr {
  long mq_flags;
  long mq_maxmsg;
  long mq_msgsize;
  long mq_curmsgs;
};

/*@
  assigns \result, errno \from indirect:mqdes;
*/
extern int mq_close(mqd_t mqdes);

/*@
  assigns \result, errno \from indirect:mqdes;
  assigns *mqstat \from indirect:mqdes;
*/
extern int mq_getattr(mqd_t mqdes, struct mq_attr *mqstat);

/*@
  assigns \result, errno \from indirect:mqdes, indirect:*notification;
*/
extern int mq_notify(mqd_t mqdes, const struct sigevent *notification);

/*@
  assigns \result, errno \from indirect:name[0..], indirect:oflag;
*/
extern mqd_t mq_open(const char *name, int oflag, ...);

/*@
  assigns msg_ptr[0 .. msg_len-1] \from indirect:mqdes, indirect:msg_len;
  assigns *msg_prio \from indirect:mqdes;
  assigns \result, errno \from indirect:mqdes, indirect:msg_len;
*/
extern ssize_t mq_receive(mqd_t mqdes, char *msg_ptr, size_t msg_len,
                          unsigned *msg_prio);

/*@
  assigns \result, errno \from indirect:mqdes,
    indirect:msg_ptr[0 .. msg_len-1],
    indirect:msg_len, indirect:msg_prio;
*/
extern int mq_send(mqd_t mqdes, const char *msg_ptr, size_t msg_len,
                   unsigned msg_prio);

/*@
  assigns \result, errno \from indirect:mqdes, indirect:*mqstat;
  assigns *omqstat \from indirect:mqdes, indirect:*mqstat;
*/
extern int mq_setattr(mqd_t mqdes, const struct mq_attr *restrict mqstat,
                      struct mq_attr *restrict omqstat);

/*@
  assigns msg_ptr[0 .. msg_len-1] \from indirect:mqdes, indirect:msg_len,
    indirect:*abstime;
  assigns *msg_prio \from indirect:mqdes, indirect:*abstime;
  assigns \result, errno \from indirect:mqdes, indirect:msg_len,
    indirect:*abstime;
*/
extern ssize_t mq_timedreceive(mqd_t mqdes, char *restrict msg_ptr,
                               size_t msg_len, unsigned *restrict msg_prio,
                               const struct timespec *restrict abstime);

/*@
  assigns \result, errno \from indirect:mqdes,
    indirect:msg_ptr[0 .. msg_len-1],
    indirect:msg_len, indirect:msg_prio, indirect:*abstime;
*/
extern int mq_timedsend(mqd_t mqdes, const char *msg_ptr, size_t msg_len,
                        unsigned msg_prio, const struct timespec *abstime);

/*@
  assigns \result, errno \from indirect:name[0..];
*/
extern int mq_unlink(const char *name);

__END_DECLS

__POP_FC_STDLIB
#endif

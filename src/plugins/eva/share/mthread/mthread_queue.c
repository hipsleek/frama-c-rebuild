/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "mthread_queue.h"

int queuecreate(msgqueue_t *q, int size) {
  *((int *)q) = Frama_C_queue_init(q, size);
  return 0;
}

int msgsnd(msgqueue_t msgqid, const char *mess, int size) {
  int result = Frama_C_queue_send(msgqid, mess, size);
  // TODO: position errno
  return result;
}

int msgrcv(msgqueue_t msgqid, int size, char *mess) {
  return Frama_C_queue_receive(msgqid, size, mess);
}

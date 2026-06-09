/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef _FRAMAC_QUEUE_H_
#define _FRAMAC_QUEUE_H_

#include <mthread.h>

typedef __fc_mthread_id msgqueue_t;

int queuecreate(msgqueue_t *q, int size);
int msgsnd(msgqueue_t msgqid, const char *mess, int size);
int msgrcv(msgqueue_t msgqid, int size, char *mess);

#endif

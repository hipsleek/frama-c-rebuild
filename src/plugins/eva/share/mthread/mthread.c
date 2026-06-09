/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include <features.h>

#ifndef MTHREAD_NUMBER_IDS
#define MTHREAD_NUMBER_IDS 32
#endif

__FC_INTERN int __fc_mthread_threads_running = 0;

__FC_INTERN int __fc_mthread_threads[MTHREAD_NUMBER_IDS];
__FC_INTERN int __fc_mthread_mutexes[MTHREAD_NUMBER_IDS];
__FC_INTERN int __fc_mthread_queues[MTHREAD_NUMBER_IDS];

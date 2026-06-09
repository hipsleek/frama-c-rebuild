/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_MTHREAD
#define __FC_MTHREAD
#include "features.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

__FC_EXTERN int __fc_mthread_shared;

typedef void *__fc_mthread_name;

typedef int __fc_mthread_id;

//@ assigns __fc_mthread_shared \from \nothing;
__fc_mthread_id Frama_C_thread_create(__fc_mthread_name, void *(*)(), ...)
    __attribute__((FC_BUILTIN));
//@ assigns __fc_mthread_shared \from \nothing;
int Frama_C_thread_start(__fc_mthread_id) __attribute__((FC_BUILTIN));

//@ assigns __fc_mthread_shared \from \nothing;
int Frama_C_thread_suspend(__fc_mthread_id) __attribute__((FC_BUILTIN));

//@ assigns __fc_mthread_shared \from \nothing;
int Frama_C_thread_cancel(__fc_mthread_id) __attribute__((FC_BUILTIN));

//@ assigns __fc_mthread_shared \from \nothing;
__fc_mthread_id Frama_C_thread_id(void) __attribute__((FC_BUILTIN));

/*@ terminates \false;
  @ assigns __fc_mthread_shared \from \nothing; */
void Frama_C_thread_exit(void *) __attribute__((FC_BUILTIN));

//@ assigns __fc_mthread_shared \from \nothing;
void Frama_C_thread_priority(int p) __attribute__((FC_BUILTIN));

//@ assigns __fc_mthread_shared \from \nothing;
__fc_mthread_id Frama_C_mutex_init(__fc_mthread_name)
    __attribute__((FC_BUILTIN));
//@ assigns __fc_mthread_shared \from \nothing;
int Frama_C_mutex_lock(__fc_mthread_id) __attribute__((FC_BUILTIN));
//@ assigns __fc_mthread_shared \from \nothing;
int Frama_C_mutex_unlock(__fc_mthread_id) __attribute__((FC_BUILTIN));

//@ assigns __fc_mthread_shared \from \nothing;
__fc_mthread_id Frama_C_queue_init(__fc_mthread_name, int)
    __attribute__((FC_BUILTIN));

/*@ requires \valid_read(buf+(0..(size-1)));
  @ assigns __fc_mthread_shared \from \nothing; */
int Frama_C_queue_send(__fc_mthread_id id, const char *buf, int size)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(buf+(0..(size-1)));
  @ assigns *buf \from \empty;
  @ assigns __fc_mthread_shared \from \nothing; */
int Frama_C_queue_receive(__fc_mthread_id, int size, char *buf)
    __attribute__((FC_BUILTIN));

//@ assigns __fc_mthread_shared \from \nothing;
void Frama_C_mthread_show(char const *, ...) __attribute__((FC_BUILTIN));

//@ assigns __fc_mthread_shared \from \nothing;
void Frama_C_mthread_sync(void) __attribute__((FC_BUILTIN));

#define __MTHREAD_SYNC(v) (Frama_C_mthread_sync(), (v))

__END_DECLS

__POP_FC_STDLIB
#endif // __FC_MTHREAD

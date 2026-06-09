/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_PTHREAD_TYPES_H
#define __FC_DEFINE_PTHREAD_TYPES_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
// These types are defined as structs with a meaningless field just to provide
// stronger typing constraints

#ifndef __have_pthread_attr_t
typedef struct __fc_pthread_attr_t { __FC_INTERN int _fc; } pthread_attr_t;
#define __have_pthread_attr_t 1
#endif

typedef struct __fc_pthread_barrier_t { __FC_INTERN int _fc; } pthread_barrier_t;
typedef struct __fc_pthread_barrierattr_t { __FC_INTERN int _fc; } pthread_barrierattr_t;
typedef struct __fc_pthread_cond_t { __FC_INTERN int _fc; } pthread_cond_t;
typedef struct __fc_pthread_condattr_t { __FC_INTERN int _fc; } pthread_condattr_t;
typedef struct __fc_pthread_key_t { __FC_INTERN int _fc; } pthread_key_t;
typedef struct __fc_pthread_mutex_t { __FC_INTERN int _fc; } pthread_mutex_t;
typedef struct __fc_pthread_mutexattr_t { __FC_INTERN int _fc; } pthread_mutexattr_t;
typedef struct __fc_pthread_once_t { __FC_INTERN int _fc; } pthread_once_t;
typedef struct __fc_pthread_rwlock_t { __FC_INTERN int _fc; } pthread_rwlock_t;
typedef struct __fc_pthread_rwlockattr_t { __FC_INTERN int _fc; } pthread_rwlockattr_t;
typedef struct __fc_pthread_spinlock_t { __FC_INTERN int _fc; } pthread_spinlock_t;
typedef unsigned long pthread_t;
__END_DECLS
__POP_FC_STDLIB
#endif

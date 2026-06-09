/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_PTHREAD_H
#define __FC_PTHREAD_H

#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include <errno.h>
#include "__fc_string_axiomatic.h"
// The values for the constants below are based on an x86 Linux,
// declared in the order given by POSIX.1-2008.

enum __fc_pthread_cancelstate
{
  PTHREAD_CANCEL_ENABLE,
  PTHREAD_CANCEL_DISABLE
};

enum __fc_pthread_canceltype
{
  PTHREAD_CANCEL_DEFERRED,
  PTHREAD_CANCEL_ASYNCHRONOUS
};

enum __fc_pthread_detachstate
{
  PTHREAD_CREATE_JOINABLE,
  PTHREAD_CREATE_DETACHED
};

enum __fc_pthread_inheritsched
{
  PTHREAD_INHERIT_SCHED,
  PTHREAD_EXPLICIT_SCHED
};


/* Scope handling.  */
enum __fc_pthread_scope
{
  PTHREAD_SCOPE_SYSTEM,
  PTHREAD_SCOPE_PROCESS
};

enum __fc_pthread_mutex_type
{
  PTHREAD_MUTEX_NORMAL,
  PTHREAD_MUTEX_RECURSIVE,
  PTHREAD_MUTEX_ERRORCHECK,
  PTHREAD_MUTEX_DEFAULT
};

enum __fc_pthread_mutex_robust
{
  PTHREAD_MUTEX_STALLED,
  PTHREAD_MUTEX_ROBUST
};

enum __fc_pthread_mutex_protocol
{
  PTHREAD_PRIO_NONE,
  PTHREAD_PRIO_INHERIT,
  PTHREAD_PRIO_PROTECT
};

enum __fc_pthread_mutex_pshared
{
  PTHREAD_PROCESS_PRIVATE,
  PTHREAD_PROCESS_SHARED
};

#define PTHREAD_BARRIER_SERIAL_THREAD (-1)
#define PTHREAD_CANCEL_ASYNCHRONOUS PTHREAD_CANCEL_ASYNCHRONOUS
#define PTHREAD_CANCEL_ENABLE PTHREAD_CANCEL_ENABLE
#define PTHREAD_CANCEL_DEFERRED PTHREAD_CANCEL_DEFERRED
#define PTHREAD_CANCEL_DISABLE PTHREAD_CANCEL_DISABLE
#define PTHREAD_CANCELED ((void *) -1)
#define PTHREAD_CREATE_DETACHED PTHREAD_CREATE_DETACHED
#define PTHREAD_CREATE_JOINABLE PTHREAD_CREATE_JOINABLE

#define PTHREAD_EXPLICIT_SCHED PTHREAD_EXPLICIT_SCHED
#define PTHREAD_INHERIT_SCHED PTHREAD_INHERIT_SCHED

#define PTHREAD_MUTEX_DEFAULT PTHREAD_MUTEX_DEFAULT
#define PTHREAD_MUTEX_ERRORCHECK PTHREAD_MUTEX_ERRORCHECK
#define PTHREAD_MUTEX_NORMAL PTHREAD_MUTEX_NORMAL
#define PTHREAD_MUTEX_RECURSIVE PTHREAD_MUTEX_RECURSIVE
#define PTHREAD_MUTEX_ROBUST PTHREAD_MUTEX_ROBUST
#define PTHREAD_MUTEX_STALLED PTHREAD_MUTEX_STALLED
#define PTHREAD_ONCE_INIT 0

#define PTHREAD_PRIO_INHERIT PTHREAD_PRIO_INHERIT

#define PTHREAD_PRIO_NONE PTHREAD_PRIO_NONE

#define PTHREAD_PRIO_PROTECT PTHREAD_PRIO_PROTECT

#define PTHREAD_PROCESS_SHARED PTHREAD_PROCESS_SHARED
#define PTHREAD_PROCESS_PRIVATE PTHREAD_PROCESS_PRIVATE

#define PTHREAD_SCOPE_PROCESS   PTHREAD_SCOPE_PROCESS
#define PTHREAD_SCOPE_SYSTEM    PTHREAD_SCOPE_SYSTEM

#define PTHREAD_COND_INITIALIZER { 0 }
#define PTHREAD_MUTEX_INITIALIZER { 0 }
#define PTHREAD_RWLOCK_INITIALIZER { 0 }

// Include pthread-related types
#include "__fc_define_pthread_types.h"
#include "__fc_define_size_t.h"
#include <sched.h>

/*@
  assigns \result \from indirect:prepare, indirect:parent, indirect:child;
*/
extern int pthread_atfork(void (*prepare)(void), void (*parent)(void),
                          void(*child)(void));

/*@
  assigns \result, *attr \from *attr;
*/
extern int pthread_attr_destroy(pthread_attr_t *attr);

/*@
  assigns \result, *detachstate \from *attr;
*/
extern int pthread_attr_getdetachstate(const pthread_attr_t *attr,
                                       int *detachstate);

/*@
  assigns \result, *guardsize \from *attr;
*/
extern int pthread_attr_getguardsize(const pthread_attr_t *restrict attr,
                                     size_t *restrict guardsize);

/*@
  assigns \result, *inheritsched \from *attr;
*/
extern int pthread_attr_getinheritsched(const pthread_attr_t *restrict attr,
                                        int *restrict inheritsched);

/*@
  assigns \result, *schedparam \from *attr;
*/
extern int pthread_attr_getschedparam(const pthread_attr_t *restrict attr,
                                      struct sched_param *restrict schedparam);

/*@
  assigns \result, *schedpolicy \from *attr;
*/
extern int pthread_attr_getschedpolicy(const pthread_attr_t *restrict attr,
                                       int *restrict schedpolicy);

/*@
  assigns \result, *contentionscope \from *attr;
*/
extern int pthread_attr_getscope(const pthread_attr_t *restrict attr,
                                 int *restrict contentionscope);

/*@
  assigns \result, *stackaddr, *stacksize \from *attr;
*/
extern int pthread_attr_getstack(const pthread_attr_t *restrict attr,
                                 void **restrict stackaddr,
                                 size_t *restrict stacksize);

/*@
  assigns \result, *stacksize \from *attr;
*/
extern int pthread_attr_getstacksize(const pthread_attr_t *restrict attr,
                                     size_t *restrict stacksize);

/*@
  assigns \result, *attr \from \nothing;
*/
extern int pthread_attr_init(pthread_attr_t *attr);

/*@
  assigns \result, *attr \from detachstate;
*/
extern int pthread_attr_setdetachstate(pthread_attr_t *attr, int detachstate);

/*@
  assigns \result, *attr \from guardsize;
*/
extern int pthread_attr_setguardsize(pthread_attr_t *attr, size_t guardsize);

/*@
  assigns \result, *attr \from inheritsched;
*/
extern int pthread_attr_setinheritsched(pthread_attr_t *attr,
                                        int inheritsched);

/*@
  assigns \result, *attr \from *attr, *schedparam;
 */
extern int pthread_attr_setschedparam(pthread_attr_t *restrict attr,
                                      const struct sched_param *restrict schedparam);

/*@
  assigns \result, *attr \from schedpolicy;
*/
extern int pthread_attr_setschedpolicy(pthread_attr_t *attr, int schedpolicy);

/*@
  assigns \result, *attr \from contentionscope;
*/
extern int pthread_attr_setscope(pthread_attr_t *attr, int contentionscope);

/*@
  assigns \result, *attr \from stackaddr, stacksize;
*/
extern int pthread_attr_setstack(pthread_attr_t *attr, void *stackaddr,
                                 size_t stacksize);

/*@
  assigns \result, *attr \from stacksize;
*/
extern int pthread_attr_setstacksize(pthread_attr_t *attr, size_t stacksize);

/*@
  assigns \result, *barrier \from *barrier;
*/
extern int pthread_barrier_destroy(pthread_barrier_t *barrier);

/*@
  assigns \result, *barrier \from *attr, count;
*/
extern int pthread_barrier_init(pthread_barrier_t *restrict barrier,
                                const pthread_barrierattr_t *restrict attr,
                                unsigned count);

/*@
  assigns \result, *barrier \from *barrier;
*/
extern int pthread_barrier_wait(pthread_barrier_t *barrier);

/*@
  assigns \result, *attr \from *attr;
*/
extern int pthread_barrierattr_destroy(pthread_barrierattr_t *attr);

/*@
  assigns \result, *pshared \from *attr;
*/
extern int pthread_barrierattr_getpshared(const pthread_barrierattr_t *restrict attr,
                                          int *restrict pshared);

/*@
  assigns \result, *attr \from \nothing;
*/
extern int pthread_barrierattr_init(pthread_barrierattr_t *attr);

/*@
  assigns \result, *attr \from pshared;
*/
extern int pthread_barrierattr_setpshared(pthread_barrierattr_t *attr, int pshared);

/*@
  assigns \result \from thread;
*/
extern int pthread_cancel(pthread_t thread);

/*@ requires valid_cond: \valid(cond);
    assigns \result \from \nothing;
    ensures success: \result == 0;
*/
extern int pthread_cond_broadcast(pthread_cond_t *cond);

/*@
  requires valid_cond: \valid(cond);
  assigns \result \from indirect:*cond;
  ensures success_or_error: \result == 0 || \result == EBUSY;
*/
extern int pthread_cond_destroy(pthread_cond_t * cond);

/*@
  requires valid_cond: \valid(cond);
  requires valid_null_attr: attr == \null || \valid_read(attr);
  assigns *cond \from *attr;
  assigns \result \from \nothing;
  ensures initialization:cond: \initialized(cond);
  ensures success: \result == 0; // at least on Linux
  // Note: "never returns an error" comes from the French manpage for pthreads
  //       (http://manpagesfr.free.fr/man/man3/pthread_cond_init.3.html)
*/
extern int pthread_cond_init(pthread_cond_t *restrict cond,
                             const pthread_condattr_t *restrict attr);

/*@
  assigns \result, *cond \from *cond;
*/
extern int pthread_cond_signal(pthread_cond_t *cond);

/*@
  assigns \result, *cond, *mutex \from *cond, *mutex, *abstime;
*/
extern int pthread_cond_timedwait(pthread_cond_t *restrict cond,
                                  pthread_mutex_t *restrict mutex,
                                  const struct timespec *restrict abstime);
/*@ requires valid_cond: \valid(cond);
    requires valid_mutex: \valid(mutex);
    assigns \result \from \nothing;
    ensures success: \result == 0;
  // Note: "never returns an error" comes from the French manpage for pthreads
  //       (http://manpagesfr.free.fr/man/man3/pthread_cond_init.3.html)
*/
extern int pthread_cond_wait(pthread_cond_t *restrict cond,
                             pthread_mutex_t *restrict mutex);

/*@
  assigns \result, *attr \from *attr;
*/
extern int pthread_condattr_destroy(pthread_condattr_t *attr);

//clockid_t not available yet
//extern int pthread_condattr_getclock(const pthread_condattr_t *restrict,
//                                     clockid_t *restrict clock_id);

/*@
  assigns \result, *pshared \from *attr;
*/
extern int pthread_condattr_getpshared(const pthread_condattr_t *restrict attr,
                                       int *restrict pshared);

/*@
  assigns \result, *attr \from \nothing;
*/
extern int pthread_condattr_init(pthread_condattr_t *attr);

//clockid_t not available yet
//extern int pthread_condattr_setclock(pthread_condattr_t *attr, clockid_t clock_id);

/*@
  assigns \result, *attr \from pshared;
*/
extern int pthread_condattr_setpshared(pthread_condattr_t *attr, int pshared);

/*@ requires valid_thread: \valid(thread);
    requires valid_null_attr: attr == \null || \valid_read(attr);
    requires valid_routine: \valid_function(start_routine);
    requires valid_null_arg: arg == \null || \valid((char*)arg);
    assigns *thread \from *attr;
    assigns \result \from indirect:*attr;
    ensures initialization:success_or_error:
      (\result == 0 && \initialized(thread)) ||
      \result == EAGAIN || \result == EINVAL || \result == EPERM;
 */
extern int pthread_create(pthread_t *restrict thread,
                          const pthread_attr_t *restrict attr,
                          void *(*start_routine)(void*),
                          void *restrict arg);

/*@
  assigns \result \from thread;
*/
extern int pthread_detach(pthread_t thread);

/*@
  assigns \result \from indirect:t1, indirect:t2;
*/
extern int pthread_equal(pthread_t t1, pthread_t t2);

/*@
  assigns ((char*)value_ptr)[0..\block_length(value_ptr)-\offset(value_ptr)-1]
    \from \nothing; //missing: \from 'current thread'
*/
extern void pthread_exit(void *value_ptr);

/*@
  assigns \result \from \nothing; //missing: \from 'current thread'
*/
extern int pthread_getconcurrency(void);

//clockid_t not available yet
//extern int pthread_getcpuclockid(pthread_t, clockid_t *);

/*@
  assigns \result, *policy, *param \from indirect:thread; //missing: \from 'thread data'
*/
extern int pthread_getschedparam(pthread_t thread, int *restrict policy,
                                 struct sched_param *restrict param);

/*@
  assigns \result \from indirect:key; //missing: \from 'current thread'
*/
extern void *pthread_getspecific(pthread_key_t key);

/*@ requires valid_or_null_retval: retval == \null || \valid(retval);
    assigns *retval \from thread;
    assigns \result \from indirect:thread;
    ensures success_or_error:
      \result == 0 ||
      \result == EDEADLK || \result == EINVAL || \result == ESRCH;

    behavior ignore_retval:
      assumes null_retval: retval == \null;
      assigns \result \from indirect:thread;

    behavior use_retval:
      assumes valid_retval: \valid(retval);
      assigns *retval \from thread;
      assigns \result \from indirect:thread;
*/
extern int pthread_join(pthread_t thread, void **retval);

/*@
  assigns \result, *key \from indirect:destruct; //missing: assigns 'current thread'
*/
extern int pthread_key_create(pthread_key_t *key, void (*destruct)(void*));

/*@
  assigns \result \from indirect:key; //missing: assigns 'current thread'
*/
extern int pthread_key_delete(pthread_key_t key);

/*@
  assigns \result, *mutex \from *mutex;
*/
extern int pthread_mutex_consistent(pthread_mutex_t *mutex);

/*@ requires mutex_valid: \valid(mutex);
  assigns *mutex \from *mutex;
  assigns \result \from indirect:*mutex;
  ensures init_or_busy: \result == 0 || \result == EBUSY;
*/
extern int pthread_mutex_destroy(pthread_mutex_t *mutex);

/*@
  assigns \result, *mutex, *prioceiling \from *mutex;
*/
extern int pthread_mutex_getprioceiling(const pthread_mutex_t *restrict mutex,
                                        int *restrict prioceiling);
/*@
  requires mutex_valid: \valid(mutex);
  requires attrs_valid_or_null: attrs == \null || \valid_read(attrs);
  assigns *mutex \from *mutex, *attrs;
  assigns \result \from indirect:*mutex, indirect:*attrs;
  // NB: under Linux, \result is guaranteed to be 0.
  ensures initialization:success_or_error:
  (\result == 0 && \initialized(mutex))
  || \result == EAGAIN
  || \result == ENOMEM
  || \result == EPERM
  || \result == EINVAL;
*/
extern int pthread_mutex_init(pthread_mutex_t *restrict mutex,
                              const pthread_mutexattr_t *restrict attrs);

/*@
  requires mutex_valid: \valid(mutex);
  assigns *mutex \from *mutex;
  assigns \result \from indirect:*mutex;
  ensures success_or_error:
  \result == 0
  || \result == EAGAIN
  || \result == EINVAL
  || \result == EDEADLK;
  // NB: more error codes are specified in POSIX, but they are not
  // exported by our version of errno.h
 */
extern int pthread_mutex_lock(pthread_mutex_t * mutex);

/*@
  assigns \result, *mutex, *old_ceiling \from *mutex, prioceiling;
*/
extern int pthread_mutex_setprioceiling(pthread_mutex_t *restrict mutex,
                                        int prioceiling,
                                        int *restrict old_ceiling);

/*@
  assigns \result, *mutex \from *mutex, *abstime;
*/
extern int pthread_mutex_timedlock(pthread_mutex_t *restrict mutex,
                                   const struct timespec *restrict abstime);

/*@
  assigns \result, *mutex \from *mutex;
*/
extern int pthread_mutex_trylock(pthread_mutex_t *mutex);

/*@
  requires mutex_valid: \valid(mutex);
  assigns *mutex \from *mutex;
  assigns \result \from indirect:*mutex;
  ensures success_or_error: \result == 0 || \result == EPERM;
*/
extern int pthread_mutex_unlock(pthread_mutex_t *mutex);

/*@
  requires valid_attr: \valid(attr);
  assigns *attr \from *attr;
  assigns \result \from indirect:*attr;
  ensures success_or_error: \result == 0 || \result == EINVAL;
*/
extern int pthread_mutexattr_destroy(pthread_mutexattr_t *attr);

/*@
  assigns \result, *prioceiling \from *attr;
*/
extern int pthread_mutexattr_getprioceiling(const pthread_mutexattr_t *restrict attr,
                                            int *restrict prioceiling);

/*@
  assigns \result, *protocol \from *attr;
*/
extern int pthread_mutexattr_getprotocol(const pthread_mutexattr_t *restrict attr,
                                         int *restrict protocol);

/*@
  assigns \result, *pshared \from *attr;
*/
extern int pthread_mutexattr_getpshared(const pthread_mutexattr_t *restrict attr,
                                        int *restrict pshared);

/*@
  assigns \result, *robust \from *attr;
*/
extern int pthread_mutexattr_getrobust(const pthread_mutexattr_t *restrict attr,
                                       int *restrict robust);

/*@
  assigns \result, *type \from *attr;
*/
extern int pthread_mutexattr_gettype(const pthread_mutexattr_t *restrict attr,
                                     int *restrict type);

/*@
  requires valid_attr: \valid(attr);
  assigns \result, *attr \from \nothing;
  ensures success_or_error: \result == 0 || \result == ENOMEM;
*/
extern int pthread_mutexattr_init(pthread_mutexattr_t *attr);

/*@
  assigns \result, *attr \from *attr, prioceiling;
*/
extern int pthread_mutexattr_setprioceiling(pthread_mutexattr_t *attr,
                                            int prioceiling);

/*@
  assigns \result, *attr \from *attr, protocol;
*/
extern int pthread_mutexattr_setprotocol(pthread_mutexattr_t *attr,
                                         int protocol);

/*@
  assigns \result, *attr \from *attr, pshared;
*/
extern int pthread_mutexattr_setpshared(pthread_mutexattr_t *attr,
                                        int pshared);

/*@
  assigns \result, *attr \from *attr, robust;
*/
extern int pthread_mutexattr_setrobust(pthread_mutexattr_t *attr,
                                       int robust);

/*@
  requires valid_attr: \valid(attr);
  assigns \result, *attr \from indirect:type;
  ensures success_or_error: \result == 0 || \result == EINVAL;
*/
extern int pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type);

/*@
  assigns \result, *pthread_once \from *pthread_once, indirect:init_routine;
*/
extern int pthread_once(pthread_once_t *pthread_once,
                        void (*init_routine)(void));

/*@
  assigns \result, *rwlock \from *rwlock;
*/
extern int pthread_rwlock_destroy(pthread_rwlock_t *rwlock);

/*@
  assigns \result, *rwlock \from *attr;
*/
extern int pthread_rwlock_init(pthread_rwlock_t *restrict rwlock,
                               const pthread_rwlockattr_t *restrict attr);

/*@
  assigns \result, *rwlock \from *rwlock;
*/
extern int pthread_rwlock_rdlock(pthread_rwlock_t *rwlock);

/*@
  assigns \result, *rwlock \from *rwlock, *abstime;
*/
extern int pthread_rwlock_timedrdlock(pthread_rwlock_t *restrict rwlock,
                                      const struct timespec *restrict abstime);

/*@
  assigns \result, *rwlock \from *rwlock, *abstime;
*/
extern int pthread_rwlock_timedwrlock(pthread_rwlock_t *restrict rwlock,
                                      const struct timespec *restrict abstime);

/*@
  assigns \result, *rwlock \from *rwlock;
*/
extern int pthread_rwlock_tryrdlock(pthread_rwlock_t *rwlock);

/*@
  assigns \result, *rwlock \from *rwlock;
*/
extern int pthread_rwlock_trywrlock(pthread_rwlock_t *rwlock);

/*@
  assigns \result, *rwlock \from *rwlock;
*/
extern int pthread_rwlock_unlock(pthread_rwlock_t *rwlock);

/*@
  assigns \result, *rwlock \from *rwlock;
*/
extern int pthread_rwlock_wrlock(pthread_rwlock_t *rwlock);

/*@
  assigns \result, *attr \from *attr;
*/
extern int pthread_rwlockattr_destroy(pthread_rwlockattr_t *attr);

/*@
  assigns \result, *pshared \from *attr;
*/
extern int pthread_rwlockattr_getpshared(const pthread_rwlockattr_t *restrict attr,
                                         int *restrict pshared);

/*@
  assigns \result, *attr \from \nothing;
*/
extern int pthread_rwlockattr_init(pthread_rwlockattr_t *attr);

/*@
  assigns \result, *attr \from *attr, pshared;
*/
extern int pthread_rwlockattr_setpshared(pthread_rwlockattr_t *attr, int pshared);

/*@
  assigns \result \from \nothing; //missing: \from 'current thread'
*/
extern pthread_t pthread_self(void);

/*@
  assigns \result, *oldstate \from state; //missing: \from 'current thread'
*/
extern int pthread_setcancelstate(int state, int *oldstate);

/*@
  assigns \result, *oldtype \from type; //missing: \from 'current thread'
*/
extern int pthread_setcanceltype(int type, int *oldtype);

/*@
  assigns \result \from indirect:new_level; //missing: assigns 'current thread'
*/
extern int pthread_setconcurrency(int new_level);

/*@
  assigns \result \from indirect:thread, indirect:policy, indirect:*param; //missing: assigns 'current thread'
*/
extern int pthread_setschedparam(pthread_t thread, int policy,
                                 const struct sched_param *param);

/*@
  assigns \result \from indirect:thread, indirect:prio; //missing: assigns 'current thread'
*/
extern int pthread_setschedprio(pthread_t thread, int prio);

/*@
  assigns \result \from indirect:key, indirect:value; //missing: assigns 'current thread data'
*/
extern int pthread_setspecific(pthread_key_t key, const void *value);

/*@
  assigns \result, *lock \from *lock;
*/
extern int pthread_spin_destroy(pthread_spinlock_t *lock);

/*@
  assigns \result, *lock \from pshared;
*/
extern int pthread_spin_init(pthread_spinlock_t *lock, int pshared);

/*@
  assigns \result, *lock \from *lock;
*/
extern int pthread_spin_lock(pthread_spinlock_t *lock);

/*@
  assigns \result, *lock \from *lock;
*/
extern int pthread_spin_trylock(pthread_spinlock_t *lock);

/*@
  assigns \result, *lock \from *lock;
*/
extern int pthread_spin_unlock(pthread_spinlock_t *lock);

/*@
  assigns \nothing; //missing: assigns 'current thread'
*/
extern void pthread_testcancel(void);

// GNU extensions

/*@ requires valid_name: valid_read_string(name);
    assigns \result \from indirect:thread, indirect:name[0 .. strlen(name)];
*/
int pthread_setname_np(pthread_t thread, const char* name);

/*@ requires valid_name: \valid(name + (0 .. len - 1));
    assigns \result, name[0 .. len - 1] \from thread;
    ensures name_written: initialization:
        \result == 0 ==>
           valid_string(name) &&
           \initialized(name + (0 .. strlen(name)));
*/
int pthread_getname_np(pthread_t thread, char* name, size_t len);

// From POSIX: "Inclusion of the <pthread.h> header shall make symbols defined
//              in the headers <sched.h> and <time.h> visible."
//              (sched.h has already been included)
#include <time.h>

__END_DECLS
__POP_FC_STDLIB
#endif

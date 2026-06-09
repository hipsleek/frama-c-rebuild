/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SPAWN_H
#define __FC_SPAWN_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_mode_t.h"
#include "__fc_define_pid_t.h"
#include "__fc_define_sigset_t.h"

__BEGIN_DECLS


typedef struct __fc_posix_spawnattr_t {
  int __fc_psa;
} posix_spawnattr_t;

typedef struct __fc_posix_posix_spawn_file_actions_t {
  int __fc_psfa;
} posix_spawn_file_actions_t;

#define POSIX_SPAWN_RESETIDS 0x01
#define POSIX_SPAWN_SETPGROUP 0x02
#define POSIX_SPAWN_SETSCHEDPARAM 0x10
#define POSIX_SPAWN_SETSCHEDULER 0x20
#define POSIX_SPAWN_SETSIGDEF 0x04
#define POSIX_SPAWN_SETSIGMASK 0x08

/*@
  assigns *pid, \result \from
    indirect:path[0..],
    indirect:*file_actions,
    indirect:*attrp,
    indirect:*argv[0..],
    indirect:*envp[0..];
*/
extern int posix_spawn(pid_t *restrict pid, const char *restrict path,
                       const posix_spawn_file_actions_t *file_actions,
                       const posix_spawnattr_t *restrict attrp,
                       char *const *restrict argv,
                       char *const *restrict envp);

/*@
  assigns \result, *file_actions \from fildes;
*/
extern int posix_spawn_file_actions_addclose(posix_spawn_file_actions_t
                                             *file_actions,
                                             int fildes);

/*@
  assigns \result, *file_actions \from fildes, newfildes;
*/
extern int posix_spawn_file_actions_adddup2(posix_spawn_file_actions_t
                                            *file_actions,
                                            int fildes, int newfildes);

/*@
  assigns \result, *file_actions
    \from fildes, path[0..], oflag, mode;
*/
extern int posix_spawn_file_actions_addopen(posix_spawn_file_actions_t
                                            *restrict file_actions,
                                            int fildes,
                                            const char *restrict path,
                                            int oflag, mode_t mode);

/*@
  assigns \result, *file_actions \from \nothing; // missing: \from 'filesystem'
*/
extern int posix_spawn_file_actions_destroy(posix_spawn_file_actions_t
                                            *file_actions);

/*@
  assigns \result, *file_actions \from \nothing; // missing: \from 'filesystem'
*/
extern int posix_spawn_file_actions_init(posix_spawn_file_actions_t
                                         *file_actions);

/*@
  assigns \result, *attr \from \nothing;
*/
extern int posix_spawnattr_destroy(posix_spawnattr_t *attr);

/*@
  assigns \result, *flags \from *attr;
*/
extern int posix_spawnattr_getflags(const posix_spawnattr_t *restrict attr,
                                    short *restrict flags);

/*@
  assigns \result, *pgroup \from *attr;
*/
extern int posix_spawnattr_getpgroup(const posix_spawnattr_t *restrict attr,
                                     pid_t *restrict pgroup);

/*@
  assigns \result, *schedparam \from *attr;
*/
extern int posix_spawnattr_getschedparam(const posix_spawnattr_t *restrict attr,
                                         struct sched_param *restrict
                                         schedparam);

/*@
  assigns \result, *schedpolicy \from *attr;
*/
extern int posix_spawnattr_getschedpolicy(const posix_spawnattr_t *restrict
                                          attr,
                                          int *restrict schedpolicy);

/*@
  assigns \result, *sigdefault \from *attr;
*/
extern int posix_spawnattr_getsigdefault(const posix_spawnattr_t *restrict attr,
                                         sigset_t *restrict sigdefault);

/*@
  assigns \result, *sigmask \from *attr;
*/
extern int posix_spawnattr_getsigmask(const posix_spawnattr_t *restrict attr,
                                      sigset_t *restrict sigmask);

/*@
  assigns \result, *attr \from \nothing;
*/
extern int posix_spawnattr_init(posix_spawnattr_t *attr);

/*@
  assigns \result, *attr \from flags;
*/
extern int posix_spawnattr_setflags(posix_spawnattr_t *attr, short flags);

/*@
  assigns \result, *attr \from pgroup;
*/
extern int posix_spawnattr_setpgroup(posix_spawnattr_t *attr, pid_t pgroup);

/*@
  assigns \result, *attr \from *schedparam;
*/
extern int posix_spawnattr_setschedparam(posix_spawnattr_t *restrict attr,
                                         const struct sched_param *restrict
                                         schedparam);

/*@
  assigns \result, *attr \from schedpolicy;
*/
extern int posix_spawnattr_setschedpolicy(posix_spawnattr_t *attr,
                                          int schedpolicy);

/*@
  assigns \result, *attr \from *sigdefault;
*/
extern int posix_spawnattr_setsigdefault(posix_spawnattr_t *restrict attr,
                                         const sigset_t *restrict sigdefault);

/*@
  assigns \result, *attr \from *sigmask;
*/
extern int posix_spawnattr_setsigmask(posix_spawnattr_t *restrict attr,
                                      const sigset_t *restrict sigmask);

/*@
  assigns *pid, \result \from
    indirect:file[0..], //missing: '\from PWD'
    indirect:*file_actions,
    indirect:*attrp,
    indirect:*argv[0..],
    indirect:*envp[0..];
*/
extern int posix_spawnp(pid_t *restrict pid, const char *restrict file,
                        const posix_spawn_file_actions_t *file_actions,
                        const posix_spawnattr_t *restrict attrp,
                        char *const *restrict argv,
                        char *const *restrict envp);

__END_DECLS

__POP_FC_STDLIB
#endif

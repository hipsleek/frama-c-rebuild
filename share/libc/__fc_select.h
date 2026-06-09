/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SELECT_H
#define __FC_SELECT_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_fd_set_t.h"
#include "__fc_define_sigset_t.h"
#include "__fc_define_suseconds_t.h"
#include "__fc_define_timeval.h"
#include "__fc_define_timespec.h"
__BEGIN_DECLS

// __fc_fds_state is a very coarse model for the state of all
// file descriptor sets; it is sound, but very imprecise.
//@ ghost __FC_EXTERN volatile int __fc_fds_state;

/*@
  requires nfds: nfds >= 0;
  requires readfs: readfds == \null || \valid(readfds);
  requires writefds: writefds == \null || \valid(writefds);
  requires errorfds: errorfds == \null || \valid(errorfds);
  requires timeout: timeout == \null || \valid(timeout);
  requires sigmask: sigmask == \null || \valid(sigmask);
  assigns __fc_fds_state \from __fc_fds_state;
  assigns *readfds, *writefds, *errorfds, *sigmask, \result
    \from indirect:nfds,
          indirect:readfds, indirect:*readfds,
          indirect:writefds, indirect:*writefds,
          indirect:errorfds, indirect:*errorfds,
          indirect:timeout, indirect:*timeout,
          indirect:sigmask, indirect:*sigmask,
          __fc_fds_state;
  behavior read_notnull:
    assumes readfds_is_not_null: readfds != \null;
    ensures initialization:readfds: \initialized(readfds);
  behavior write_notnull:
    assumes writefds_is_not_null: writefds != \null;
    ensures initialization:writefds: \initialized(writefds);
  behavior error_notnull:
    assumes errorfds_is_not_null: errorfds != \null;
    ensures initialization:errorfds: \initialized(errorfds);
  behavior timeout_notnull:
    assumes timeout_is_not_null: timeout != \null;
    ensures initialization:timeout: \initialized(timeout);
 */
extern int pselect(int nfds, fd_set *readfds, fd_set *writefds,
                   fd_set *errorfds, const struct timespec *timeout,
                   const sigset_t *sigmask);

/*@
  requires nfds: nfds >= 0;
  requires readfs: readfds == \null || \valid(readfds);
  requires writefds: writefds == \null || \valid(writefds);
  requires errorfds: errorfds == \null || \valid(errorfds);
  requires timeout: timeout == \null || \valid(timeout);
  assigns __fc_fds_state \from __fc_fds_state;
  assigns *readfds, *writefds, *errorfds, *timeout, \result
    \from indirect:nfds,
          indirect:readfds, indirect:*readfds,
          indirect:writefds, indirect:*writefds,
          indirect:errorfds, indirect:*errorfds,
          indirect:timeout, indirect:*timeout,
          __fc_fds_state;
  behavior read_notnull:
    assumes readfds_is_not_null: readfds != \null;
    ensures initialization:readfds: \initialized(readfds);
  behavior write_notnull:
    assumes writefds_is_not_null: writefds != \null;
    ensures initialization:writefds: \initialized(writefds);
  behavior error_notnull:
    assumes errorfds_is_not_null: errorfds != \null;
    ensures initialization:errorfds: \initialized(errorfds);
  behavior timeout_notnull:
    assumes timeout_is_not_null: timeout != \null;
    ensures initialization:timeout: \initialized(timeout);
 */
extern int select(int nfds, fd_set * readfds,
       fd_set * writefds, fd_set * errorfds,
       struct timeval * timeout);

__END_DECLS

__POP_FC_STDLIB
#endif

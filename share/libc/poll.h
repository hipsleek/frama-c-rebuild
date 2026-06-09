/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_POLL_H
#define __FC_POLL_H
#include "features.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

struct pollfd {
  int fd; // input parameter in poll()
  short events; // input parameter in poll()
  short revents; // output parameter in poll()
};

typedef unsigned long nfds_t;

extern volatile int Frama_C_entropy_source;

// The values used below are based on Linux.
#define POLLIN     0x001
#define POLLPRI    0x002
#define POLLOUT    0x004
#define POLLERR    0x008
#define POLLHUP    0x010
#define POLLNVAL   0x020
#define POLLRDNORM 0x040
#define POLLRDBAND 0x080
#define POLLWRNORM 0x100
#define POLLWRBAND 0x200

/*@
  requires valid_file_descriptors: \valid(fds+(0 .. nfds-1));
  assigns fds[0 .. nfds-1].revents \from indirect:fds[0 .. nfds-1].fd,
                                       fds[0 .. nfds-1].events,
                                       indirect:nfds, indirect:timeout,
                                       indirect:Frama_C_entropy_source;
  assigns \result \from indirect:fds[0 .. nfds-1].fd,
                        indirect:fds[0 .. nfds-1].events,
                        indirect:nfds, indirect:timeout,
                        indirect:Frama_C_entropy_source;
  ensures error_timeout_or_bounded:
     \result == -1 || \result == 0 || 1 <= \result <= nfds;
  ensures initialization:revents: \initialized(&fds[0 .. nfds-1].revents);
 */
extern int poll (struct pollfd *fds, nfds_t nfds, int timeout);

__END_DECLS

__POP_FC_STDLIB
#endif

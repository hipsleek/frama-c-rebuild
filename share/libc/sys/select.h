/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_SELECT_H
#define __FC_SYS_SELECT_H
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "../__fc_select.h"

/*@
  requires valid_fdset: \valid(fdset);
  requires initialization: \initialized(fdset);
  assigns *fdset \from *fdset, indirect:fd;
*/
extern void FD_CLR(int fd, fd_set *fdset);
#define FD_CLR FD_CLR

// Note: the 2nd argument in FD_ISSET is not const in some implementations
// due to historical and compatibility reasons.
/*@
  requires valid_fdset: \valid_read(fdset);
  requires initialization: \initialized(fdset);
  assigns \result \from indirect:*fdset, indirect:fd;
*/
extern int FD_ISSET(int fd, const fd_set *fdset);
#define FD_ISSET FD_ISSET

/*@
  requires valid_fdset: \valid(fdset);
  requires initialization: \initialized(fdset);
  assigns *fdset \from *fdset, indirect:fd;
*/
extern void FD_SET(int fd, fd_set *fdset);
#define FD_SET FD_SET

/*@
  requires valid_fdset: \valid(fdset);
  assigns *fdset \from \nothing;
  ensures initialization: \initialized(fdset);
*/
extern void FD_ZERO(fd_set *fdset);
#define FD_ZERO FD_ZERO

__END_DECLS
__POP_FC_STDLIB
#endif

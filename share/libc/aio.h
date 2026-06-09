/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_AIO_H
#define __FC_AIO_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_off_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_ssize_t.h"
#include "__fc_define_timespec.h"
#include <errno.h>
#include <signal.h>

__BEGIN_DECLS

struct aiocb {
  int aio_fildes;
  off_t aio_offset;
  volatile void *aio_buf;
  size_t aio_nbytes;
  int aio_reqprio;
  struct sigevent aio_sigevent;
  int aio_lio_opcode;
};

#define AIO_ALLDONE 2
#define AIO_CANCELED 0
#define AIO_NOTCANCELED 1

#define LIO_NOP 2
#define LIO_NOWAIT 1

#define LIO_READ 0
#define LIO_WAIT 0
#define LIO_WRITE 1

/*@
  assigns errno, \result \from fildes, indirect:*aiocbp;
  assigns *aiocbp \from fildes, *aiocbp;
*/
extern int aio_cancel(int fildes, struct aiocb *aiocbp);

/*@
  assigns errno, \result \from indirect:*aiocbp;
*/
extern int aio_error(const struct aiocb *aiocbp);

/*@
  assigns errno, \result \from indirect:op, indirect:*aiocbp;
  assigns *aiocbp \from op, *aiocbp;
*/
extern int aio_fsync(int op, struct aiocb *aiocbp);

/*@
  assigns errno, \result \from indirect:*aiocbp;
  assigns *aiocbp \from *aiocbp;
*/
extern int aio_read(struct aiocb *aiocbp);

/*@
  assigns errno, \result \from indirect:*aiocbp;
  assigns *aiocbp \from *aiocbp;
*/
extern ssize_t aio_return(struct aiocb *aiocbp);

/*@
  assigns errno, \result \from indirect:*list[0 .. nent-1], indirect:nent,
                               indirect:*timeout;
*/
extern int aio_suspend(const struct aiocb *const list[], int nent,
                       const struct timespec *timeout);

/*@
  assigns errno, \result \from indirect:*aiocbp;
  assigns *aiocbp \from *aiocbp;
*/
extern int aio_write(struct aiocb *aiocbp);

/*@
  assigns errno, \result \from indirect:mode, indirect:*list[0 .. nent-1],
                               indirect:nent, indirect:*sig;
  assigns *sig \from indirect:mode, indirect:*list[0 .. nent-1], indirect:nent,
                     *sig;
*/
extern int lio_listio(int mode, struct aiocb *restrict const list[restrict],
                      int nent, struct sigevent *restrict sig);

__END_DECLS

__POP_FC_STDLIB
#endif

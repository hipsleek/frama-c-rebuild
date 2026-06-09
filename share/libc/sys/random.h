/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_RANDOM_H
#define __FC_SYS_RANDOM_H
#include "../features.h"
#include "../__fc_define_size_t.h"
#include "../__fc_define_ssize_t.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

/*@
  assigns \result, ((char*)buffer)[0 .. length-1] \from flags;
*/
extern ssize_t getrandom (void *buffer, size_t length,
                          unsigned int flags);

/*@
  assigns \result, ((char*)buffer)[0 .. length-1] \from \nothing;
*/
extern int getentropy (void *buffer, size_t length);

// Non-POSIX
#define GRND_NONBLOCK 0x01
#define GRND_RANDOM 0x02
#define GRND_INSECURE 0x04

__END_DECLS

__POP_FC_STDLIB
#endif

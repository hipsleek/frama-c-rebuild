/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_DEFINE_SYS_WAIT_MACROS_H
#define __FC_DEFINE_SYS_WAIT_MACROS_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"
__BEGIN_DECLS

// The values for the constants/macros below are based on the glibc on
// an x86 Linux
#define WCOREDUMP(status)    ((status) & 0x80)
#define WEXITSTATUS(status)  (((status) & 0xff00) >> 8)
#define WIFCONTINUED(status) ((status) == 0xffff)
#define WIFEXITED(status)    (((status) & 0x7f) == 0)
#define WIFSIGNALED(status)  (((signed char) (((status) & 0x7f) + 1) >> 1) > 0)
#define WIFSTOPPED(status)   (((status) & 0xff) == 0x7f)
#define WNOHANG    1
#define WSTOPSIG(status)     WEXITSTATUS(status)
#define WTERMSIG(status)     ((status) & 0x7f)
#define WUNTRACED  2

__END_DECLS
__POP_FC_STDLIB
#endif

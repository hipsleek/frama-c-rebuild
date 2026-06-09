/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/* Non-POSIX; glibc definitions */

#ifndef __FC_ERROR_H
#define __FC_ERROR_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"

__BEGIN_DECLS

extern unsigned int error_message_count;

extern int error_one_per_line;

/*@
  assigns error_message_count \from error_message_count;
*/
extern void error(int __status, int __errnum, const char *__format, ...);

/*@
  assigns error_message_count \from error_message_count;
*/
extern void error_at_line(int __status, int __errnum, const char *__fname,
                          unsigned int __lineno, const char *__format, ...);

extern void (*error_print_progname)(void);

__END_DECLS

__POP_FC_STDLIB
#endif

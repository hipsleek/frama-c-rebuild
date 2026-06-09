/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_FMTMSG_H
#define __FC_FMTMSG_H
#include "features.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

#define MM_HARD 0x001
#define MM_SOFT 0x002
#define MM_FIRM 0x004
#define MM_APPL MM_APPL
#define MM_UTIL 0x010
#define MM_OPSYS 0x020
#define MM_RECOVER 0x040
#define MM_NRECOV 0x080
#define MM_HALT 1
#define MM_ERROR 2
#define MM_WARNING 3
#define MM_INFO 4
#define MM_NOSEV 0
#define MM_PRINT 0x100
#define MM_CONSOLE 0x200

#define MM_NULLLBL ((char*)0)
#define MM_NULLSEV 0
#define MM_NULLMC 0L
#define MM_NULLTXT ((char*)0)
#define MM_NULLACT ((char*)0)
#define MM_NULLTAG ((char*)0)

#define MM_OK 0
#define MM_NOTOK (-1)
#define MM_NOMSG 1
#define MM_NOCON 4

/*@
  // missing: assigns 'device' \from 'hardware'
  assigns \result \from indirect:classification, indirect:label[0..],
                        indirect:severity, indirect:text[0..],
                        indirect:action[0..], indirect:tag[0..];
 */
extern int fmtmsg(long classification, const char *label, int severity,
                  const char *text, const char *action, const char *tag);

__END_DECLS

__POP_FC_STDLIB
#endif

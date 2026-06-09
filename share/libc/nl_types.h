/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_NL_TYPES_H
#define __FC_NL_TYPES_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

typedef unsigned long nl_catd;
typedef unsigned long nl_item;
#define NL_SETD 1
#define NL_CAT_LOCALE 1

/*@
  assigns \result \from catd;
*/
extern int catclose(nl_catd catd);

__FC_EXTERN char __fc_catgets[256]; // arbitrary size

/*@
  assigns \result \from &__fc_catgets, s, indirect:catd, indirect:set_id,
  indirect:msg_id;
  assigns __fc_catgets[0..] \from __fc_catgets[0..];
*/
extern char *catgets(nl_catd catd, int set_id, int msg_id, const char *s);

/*@
  assigns \result \from name[0..], oflag;
*/
extern nl_catd catopen(const char *name, int oflag);

__END_DECLS

__POP_FC_STDLIB
#endif

/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_SYS_UTSNAME_H
#define __FC_SYS_UTSNAME_H

#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

// Arbitrary length, based on the one used in Linux
#define _FC_UTSNAME_LENGTH 65

struct utsname
{
  char sysname[_FC_UTSNAME_LENGTH];
  char nodename[_FC_UTSNAME_LENGTH];
  char release[_FC_UTSNAME_LENGTH];
  char version[_FC_UTSNAME_LENGTH];
  char machine[_FC_UTSNAME_LENGTH];
};

/*@ // missing: assigns *name, \result \from "system information"
  requires valid_name: \valid(name);
  assigns *name, \result \from \nothing;
  ensures result_ok_or_error: -1 <= \result;
  ensures initialization:name:\initialized(name);
*/
extern int uname (struct utsname *name);

__END_DECLS
__POP_FC_STDLIB
#endif

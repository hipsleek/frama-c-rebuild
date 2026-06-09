/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifdef _WIN32
/* Must be the first included header */
#include "windows.h"
#endif

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/mlvalues.h"
#include <assert.h>
#include <stdint.h>
#include <unistd.h>

value address_of_value(value v) {
  return (Val_long(((unsigned long)v) / sizeof(long)));
}

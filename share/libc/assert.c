/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "__fc_builtin.h"
#include "assert.h"
__PUSH_FC_STDLIB

//@ assigns \nothing;
extern void Frama_C_show_each_warning(char const *, ...);

void __FC_assert(int c,const char* file,int line,const char*expr) {
  if (!c) {
#ifdef __FRAMAC__
    Frama_C_show_each_warning("Assertion may fail",file,line,expr);
#endif
    Frama_C_abort ();
  }
}

__POP_FC_STDLIB

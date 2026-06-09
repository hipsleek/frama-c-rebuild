/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

// NOTE: DO NOT INCLUDE THIS FILE DIRECTLY FROM THE COMMAND-LINE; IT IS
// ALREADY INCLUDED BY THE .C FILES THAT NEED IT.

#ifndef __FC_FILE_C
#define __FC_FILE_C

#include "__fc_builtin.h"
#include "stdio.h"
__PUSH_FC_STDLIB

// Initializers for FILE* streams used by several libc files

FILE __fc_initial_stdout = {.__fc_FILE_id=1};
FILE * __fc_stdout = &__fc_initial_stdout;

FILE __fc_initial_stderr = {.__fc_FILE_id=2};
FILE * __fc_stderr = &__fc_initial_stderr;

FILE __fc_initial_stdin = {.__fc_FILE_id=0};
FILE * __fc_stdin = &__fc_initial_stdin;

FILE __fc_fopen[__FC_FOPEN_MAX];

__POP_FC_STDLIB
#endif

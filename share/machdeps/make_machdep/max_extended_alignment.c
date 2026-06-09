/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#if !defined(ALIGN_TEST)
#define ALIGN_TEST 0
#endif

int _Alignas(ALIGN_TEST) x = 42;

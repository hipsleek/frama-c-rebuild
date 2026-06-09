/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
#include <stddef.h>
#define TEST_TYPE wchar_t

TEST_TYPE_IS(unsigned short)
TEST_TYPE_IS(short)
TEST_TYPE_IS(unsigned int)
TEST_TYPE_IS(int)
TEST_TYPE_IS(unsigned long)
TEST_TYPE_IS(long)

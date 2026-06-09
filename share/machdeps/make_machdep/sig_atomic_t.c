/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
#include <signal.h>

#define TEST_TYPE sig_atomic_t

TEST_TYPE_IS(char)
TEST_TYPE_IS(unsigned char)
TEST_TYPE_IS(signed char)
TEST_TYPE_IS(unsigned short)
TEST_TYPE_IS(short)
TEST_TYPE_IS(unsigned int)
TEST_TYPE_IS(int)
TEST_TYPE_IS(unsigned long)
TEST_TYPE_IS(long)
TEST_TYPE_IS(unsigned long long)
TEST_TYPE_IS(long long)

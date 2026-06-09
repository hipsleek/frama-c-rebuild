/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
#include <time.h>

#define TEST_TYPE time_t

TEST_TYPE_IS(unsigned int)
TEST_TYPE_IS(int)
TEST_TYPE_IS(unsigned long)
TEST_TYPE_IS(long)
TEST_TYPE_IS(unsigned long long)
TEST_TYPE_IS(long long)

// Technically, C standard speaks of a 'real' type, not an 'integer' one

TEST_TYPE_IS(float)
TEST_TYPE_IS(double)
TEST_TYPE_IS(long double)

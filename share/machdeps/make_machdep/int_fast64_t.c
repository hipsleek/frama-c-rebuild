/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
#include <stdint.h>
#define TEST_TYPE int_fast64_t

TEST_TYPE_IS(signed char);
TEST_TYPE_IS(short);
TEST_TYPE_IS(int);
TEST_TYPE_IS(long);
TEST_TYPE_IS(long long);

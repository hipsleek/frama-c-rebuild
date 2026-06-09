/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "make_machdep_common.h"
#include <stddef.h>

#define TEST_MAX_ALIGN_T_IS(type) \
    _Static_assert(ALIGNOF(max_align_t) != ALIGNOF(type), \
                   "max_align_t is `"#type"`");

TEST_MAX_ALIGN_T_IS(char)
TEST_MAX_ALIGN_T_IS(short)
TEST_MAX_ALIGN_T_IS(int)
TEST_MAX_ALIGN_T_IS(long)
TEST_MAX_ALIGN_T_IS(long long)
TEST_MAX_ALIGN_T_IS(double)
TEST_MAX_ALIGN_T_IS(long double)
TEST_MAX_ALIGN_T_IS(struct __machdep_max_align_t {int __max_align; } __attribute__ ((aligned (8))))
TEST_MAX_ALIGN_T_IS(struct __machdep_max_align_t {int __max_align; } __attribute__ ((aligned (16))))

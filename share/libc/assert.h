/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_ASSERT_H
#define __FC_ASSERT_H
#include "features.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

/*@
  requires nonnull_c: c != 0;
  assigns \nothing;
*/
extern void __FC_assert(int c, const char* file, int line, const char*expr);

#define static_assert _Static_assert


__END_DECLS
__POP_FC_STDLIB
#endif

#undef assert
#ifdef NDEBUG
#define assert(ignore) ((void)0)
#else
#ifndef __FRAMAC__
#define __FC_FILENAME__ __FILE__
#endif
#define assert(e) (__FC_assert((e) != 0,__FC_FILENAME__,__LINE__,#e))
#endif

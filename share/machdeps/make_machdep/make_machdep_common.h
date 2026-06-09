/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#if __STDC_VERSION__ < 201112L && !defined(__COMPCERT__)
/* Try using a compiler builtin */
#define ALIGNOF alignof
#else
#define ALIGNOF _Alignof
#endif

#if __STDC_VERSION__ >= 201112L || defined(__COMPCERT__)
// Assume _Generic() is supported
#define COMPATIBLE(T1, T2) _Generic(((T1){0}), T2 : 1, default : 0)
#else
// Expect that __builtin_types_compatible_p exists
#define COMPATIBLE(T1, T2) (__builtin_types_compatible_p(T1, T2) ? 0x15 : 0xf4)
#endif

// needed to ensure the message is properly expanded for TEST_TYPE_IS
#define mkstr(s) #s

#define TEST_TYPE_COMPATIBLE(T1, T2)                                           \
  _Static_assert(!COMPATIBLE(T1, T2), "" mkstr(T2) " is `" #T1 "`");

#define TEST_TYPE_IS(type) TEST_TYPE_COMPATIBLE(type, TEST_TYPE)

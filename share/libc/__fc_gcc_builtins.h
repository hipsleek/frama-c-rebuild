/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

// This file contains some GCC builtins which are not already hardcoded in
// Frama-C, and which can be expressed using ACSL.

#ifndef __FC_GCC_BUILTINS_H
#define __FC_GCC_BUILTINS_H
#include "__fc_machdep.h"
#include "features.h"

__PUSH_FC_STDLIB

__BEGIN_DECLS

#if !defined(__int128_t) && defined(__GNUC__)
typedef __int128 __int128_t;
#define __int128_t __int128_t
#endif

#if !defined(__uint128_t) && defined(__GNUC__)
typedef unsigned __int128 __uint128_t;
#define __uint128_t __uint128_t
#endif

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (int)(a + b);
  ensures result_overflow: a + b == (int)(a + b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_sadd_overflow(int a, int b, int* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long)(a + b);
  ensures result_overflow: a + b == (long)(a + b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_saddl_overflow(long a, long b, long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long long)(a + b);
  ensures result_overflow: a + b == (long long)(a + b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_saddll_overflow(long long a, long long b, long long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned)(a + b);
  ensures result_overflow: a + b == (unsigned)(a + b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_uadd_overflow(unsigned a, unsigned b, unsigned* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long)(a + b);
  ensures result_overflow: a + b == (unsigned long)(a + b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_uaddl_overflow(unsigned long a, unsigned long b, unsigned long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long long)(a + b);
  ensures result_overflow: a + b == (unsigned long long)(a + b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_uaddll_overflow(unsigned long long a, unsigned long long b, unsigned long long* res);

// NB: technically, the types of a, b and *res could differ, but we assume that
// this is not the case

#define __builtin_add_overflow(a,b,res) \
  _Generic((a), \
    int: __builtin_sadd_overflow, \
    unsigned int: __builtin_uadd_overflow, \
    long: __builtin_saddl_overflow, \
    unsigned long: __builtin_uaddl_overflow, \
    long long: __builtin_saddll_overflow, \
    unsigned long long: __builtin_uaddll_overflow \
  )(a,b,res)

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (int)(a - b);
  ensures result_overflow: a - b == (int)(a - b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_ssub_overflow(int a, int b, int* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long)(a - b);
  ensures result_overflow: a - b == (long)(a - b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_ssubl_overflow(long a, long b, long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long long)(a - b);
  ensures result_overflow: a - b == (long long)(a - b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_ssubll_overflow(long long a, long long b, long long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned)(a - b);
  ensures result_overflow: a - b == (unsigned)(a - b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_usub_overflow(unsigned a, unsigned b, unsigned* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long)(a - b);
  ensures result_overflow: a - b == (unsigned long)(a - b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_usubl_overflow(unsigned long a, unsigned long b, unsigned long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long long)(a - b);
  ensures result_overflow: a - b == (unsigned long long)(a - b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_usubll_overflow(unsigned long long a, unsigned long long b, unsigned long long* res);

// NB: technically, the types of a, b and *res could differ, but we assume that
// this is not the case

#define __builtin_sub_overflow(a,b,res) \
  _Generic((a), \
    int: __builtin_ssub_overflow, \
    unsigned int: __builtin_usub_overflow, \
    long: __builtin_ssubl_overflow, \
    unsigned long: __builtin_usubl_overflow, \
    long long: __builtin_ssubll_overflow, \
    unsigned long long: __builtin_usubll_overflow \
  )(a,b,res)

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (int)(a * b);
  ensures result_overflow: a * b == (int)(a * b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_smul_overflow(int a, int b, int* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long)(a * b);
  ensures result_overflow: a * b == (long)(a * b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_smull_overflow(long a, long b, long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long long)(a * b);
  ensures result_overflow: a * b == (long long)(a * b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_smulll_overflow(long long a, long long b, long long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned)(a * b);
  ensures result_overflow: a * b == (unsigned)(a * b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_umul_overflow(unsigned a, unsigned b, unsigned* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long)(a * b);
  ensures result_overflow: a * b == (unsigned long)(a * b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_umull_overflow(unsigned long a, unsigned long b, unsigned long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long long)(a * b);
  ensures result_overflow: a * b == (unsigned long long)(a * b) ? \result == 0 : \result == 1;
 */
__FC_BOOL __builtin_umulll_overflow(unsigned long long a, unsigned long long b, unsigned long long* res);

// NB: technically, the types of a, b and *res could differ, but we assume that
// this is not the case

#define __builtin_mul_overflow(a,b,res) \
  _Generic((a), \
    int: __builtin_smul_overflow, \
    unsigned int: __builtin_umul_overflow, \
    long: __builtin_smull_overflow, \
    unsigned long: __builtin_umull_overflow, \
    long long: __builtin_smulll_overflow, \
    unsigned long long: __builtin_umulll_overflow \
  )(a,b,res)

/*@
  requires x_nonzero: x != 0;
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result < __CHAR_BIT * sizeof(x);
 */
int __builtin_clz(unsigned int x);

/*@
  requires x_nonzero: x != 0;
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result < __CHAR_BIT * sizeof(x);
 */
int __builtin_clzl(unsigned long x);

/*@
  requires x_nonzero: x != 0;
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result < __CHAR_BIT * sizeof(x);
 */
int __builtin_clzll(unsigned long long x);

/*@
  requires x_nonzero: x != 0;
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result < __CHAR_BIT * sizeof(x);
 */
int __builtin_ctz(unsigned int x);

/*@
  requires x_nonzero: x != 0;
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result < __CHAR_BIT * sizeof(x);
 */
int __builtin_ctzl(unsigned long x);

/*@
  requires x_nonzero: x != 0;
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result < __CHAR_BIT * sizeof(x);
 */
int __builtin_ctzll(unsigned long long x);

/*@
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result <= __CHAR_BIT * sizeof(x);
 */
int __builtin_popcount(unsigned int x);

/*@
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result <= __CHAR_BIT * sizeof(x);
 */
int __builtin_popcountl(unsigned long x);

/*@
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result <= __CHAR_BIT * sizeof(x);
 */
int __builtin_popcountll(unsigned long long x);

// GCC specialized __atomic_* functions.
// TODO: add generic counterpart with some help from cabs2cil for typing
// NB: __atomic_* operations do not seem to make a distinction between
// signed and unsigned: this may lead to some warnings by Frama-C

/*@
  requires validity: \valid_read(mem);
  requires initialization: \initialized(mem);
  assigns \result \from *mem, indirect:model;
  ensures load_value: \result == *mem;
*/
__UINT8_T __atomic_load_1(const __UINT8_T* mem, int model);

/*@
  requires validity: \valid_read(mem);
  requires initialization: \initialized(mem);
  assigns \result \from *mem, indirect: model;
  ensures load_value: \result == *mem;
*/
__UINT16_T __atomic_load_2(const __UINT16_T* mem, int model);

/*@
  requires validity: \valid_read(mem);
  requires initialization: \initialized(mem);
  assigns \result \from *mem, indirect:model;
  ensures load_value: \result == *mem;
*/
__UINT32_T __atomic_load_4(const __UINT32_T* mem, int model);

/*@
  requires validity: \valid_read(mem);
  requires initialization: \initialized(mem);
  assigns \result \from *mem, indirect:model;
  ensures load_value: \result == *mem;
*/
__UINT64_T __atomic_load_8(const __UINT64_T* mem, int model);

/*@
  requires validity: \valid(mem);
  assigns *mem \from val, indirect: model;
  ensures initialization: \initialized(mem);
  ensures store_value: *mem == val;
*/
void __atomic_store_1(__UINT8_T* mem, __UINT8_T val, int model);

/*@
  requires validity: \valid(mem);
  assigns *mem \from val, indirect: model;
  ensures initialization: \initialized(mem);
  ensures store_value: *mem == val;
*/
void __atomic_store_2(__UINT16_T* mem, __UINT16_T val, int model);

/*@
  requires validity: \valid(mem);
  assigns *mem \from val, indirect: model;
  ensures initialization: \initialized(mem);
  ensures store_value: *mem == val;
*/
void __atomic_store_4(__UINT32_T* mem, __UINT32_T val, int model);

/*@
  requires validity: \valid(mem);
  assigns *mem \from val, indirect: model;
  ensures initialization: \initialized(mem);
  ensures store_value: *mem == val;
*/
void __atomic_store_8(__UINT64_T* mem, __UINT64_T val, int model);

/*@ requires validity: \valid(mem);
    requires initialization: \initialized(mem);
    assigns *mem \from val, indirect:model;
    assigns \result \from *mem, indirect:model;
 */
__UINT8_T __atomic_exchange_1(__UINT8_T* mem, __UINT8_T val, int model);

/*@ requires validity: \valid(mem);
    requires initialization: \initialized(mem);
    assigns *mem \from val, indirect:model;
    assigns \result \from *mem, indirect:model;
 */
__UINT16_T __atomic_exchange_2(__UINT16_T* mem, __UINT16_T val, int model);

/*@ requires validity: \valid(mem);
    requires initialization: \initialized(mem);
    assigns *mem \from val, indirect:model;
    assigns \result \from *mem, indirect:model;
 */
__UINT32_T __atomic_exchange_4(__UINT32_T* mem, __UINT32_T val, int model);

/*@ requires validity: \valid(mem);
    requires initialization: \initialized(mem);
    assigns *mem \from val, indirect:model;
    assigns \result \from *mem, indirect:model;
 */
__UINT64_T __atomic_exchange_8(__UINT64_T* mem, __UINT64_T val, int model);

/*@
  requires validity: \valid(mem) && \valid(expected);
  requires initialization:mem: \initialized(mem);
  requires initialization:expected: \initialized(expected);
  assigns *mem \from *mem, desired, indirect: *expected,
          indirect: success_model, indirect: weak;
  assigns *expected \from *expected, *mem, indirect: desired,
          indirect: failure_model, indirect: weak;
  assigns \result \from indirect: *mem, indirect: *expected;
*/
__FC_BOOL __atomic_compare_exchange_1(__UINT8_T* mem,
                                  __UINT8_T* expected,
                                  __UINT8_T desired,
                                  __FC_BOOL weak,
                                  int success_model,
                                  int failure_model);

/*@
  requires validity: \valid(mem) && \valid(expected);
  requires initialization:mem: \initialized(mem);
  requires initialization:expected: \initialized(expected);
  assigns *mem \from *mem, desired, indirect: *expected,
          indirect: success_model, indirect: weak;
  assigns *expected \from *expected, *mem, indirect: desired,
          indirect: failure_model, indirect: weak;
  assigns \result \from indirect: *mem, indirect: *expected;
*/
__FC_BOOL __atomic_compare_exchange_2(__UINT16_T* mem,
                                  __UINT16_T* expected,
                                  __UINT16_T desired,
                                  __FC_BOOL weak,
                                  int success_model,
                                  int failure_model);

/*@
  requires validity: \valid(mem) && \valid(expected);
  requires initialization:mem: \initialized(mem);
  requires initialization:expected: \initialized(expected);
  assigns *mem \from *mem, desired, indirect: *expected,
          indirect: success_model, indirect: weak;
  assigns *expected \from *expected, *mem, indirect: desired,
          indirect: failure_model, indirect: weak;
  assigns \result \from indirect: *mem, indirect: *expected;
*/
__FC_BOOL __atomic_compare_exchange_4(__UINT32_T* mem,
                                  __UINT32_T* expected,
                                  __UINT32_T desired,
                                  __FC_BOOL weak,
                                  int success_model,
                                  int failure_model);

/*@
  requires validity: \valid(mem) && \valid(expected);
  requires initialization:mem: \initialized(mem);
  requires initialization:expected: \initialized(expected);
  assigns *mem \from *mem, desired, indirect: *expected,
          indirect: success_model, indirect: weak;
  assigns *expected \from *expected, *mem, indirect: desired,
          indirect: failure_model, indirect: weak;
  assigns \result \from indirect: *mem, indirect: *expected;
*/
__FC_BOOL __atomic_compare_exchange_8(__UINT64_T* mem,
                                  __UINT64_T* expected,
                                  __UINT64_T desired,
                                  __FC_BOOL weak,
                                  int success_model,
                                  int failure_model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_add_fetch_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_sub_fetch_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_and_fetch_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_xor_fetch_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_or_fetch_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_nand_fetch_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_add_fetch_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_sub_fetch_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_and_fetch_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_xor_fetch_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_or_fetch_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_nand_fetch_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_add_fetch_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_sub_fetch_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_and_fetch_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_xor_fetch_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_or_fetch_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_nand_fetch_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_add_fetch_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_sub_fetch_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_and_fetch_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_xor_fetch_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_or_fetch_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_nand_fetch_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_fetch_add_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_fetch_sub_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_fetch_and_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_fetch_xor_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_fetch_or_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_fetch_nand_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_fetch_add_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_fetch_sub_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_fetch_and_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_fetch_xor_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_fetch_or_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_fetch_nand_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_fetch_add_4(volatile __UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_fetch_sub_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_fetch_and_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_fetch_xor_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_fetch_or_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_fetch_nand_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_fetch_add_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_fetch_sub__8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_fetch_and_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_fetch_xor_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_fetch_or_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_fetch_nand_8(__UINT64_T* ptr, __UINT64_T val, int model);

#if !defined(__clang__)

/*@ requires validity: \valid((char*)ptr);
    assigns \result \from *((char*)ptr);
    assigns *((char*)ptr) \from \nothing;
*/
__FC_BOOL __atomic_test_and_set(void* ptr, int model);

/*@ requires validity: \valid(ptr);
    assigns *ptr \from \nothing;
*/
void __atomic_clear(__FC_BOOL* ptr, int model);

#endif

/*@ assigns \nothing; */
void __atomic_thread_fence(int model);

/*@ assigns \nothing; */
void __atomic_signal_fence(int model);

/*@ assigns \result \from indirect: size, indirect: ptr; */
__FC_BOOL __atomic_always_lock_free(__SIZE_T size, const volatile void* ptr);

/*@ assigns \result \from indirect: size, indirect: ptr; */
__FC_BOOL __atomic_is_lock_free(__SIZE_T size, const volatile void* ptr);

// According to the GCC docs
// (https://gcc.gnu.org/onlinedocs/gcc/_005f_005fsync-Builtins.html),
// this creates a 'full memory barrier'; we do not model the concurrency
// aspects yet.
/*@ assigns \nothing; */
void __sync_synchronize(void);

__END_DECLS

__POP_FC_STDLIB
#endif

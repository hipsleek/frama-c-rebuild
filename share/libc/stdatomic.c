/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "__fc_builtin.h"
#include "stdatomic.h"
#include "assert.h"
#include "string.h"
__PUSH_FC_STDLIB

void __fc_atomic_init_marker(void *obj, unsigned long long value) {}

void atomic_thread_fence(memory_order order) {}

void atomic_signal_fence(memory_order order) {}

_Bool __fc_atomic_is_lock_free(void *obj, size_t obj_size) {
  return Frama_C_nondet(0, 1);
}

void __fc_atomic_store_marker(void *object, unsigned long long desired,
                              size_t obj_size) {}

void __fc_atomic_store_explicit_marker(void *object,
                                       unsigned long long desired,
                                       memory_order order, size_t obj_size) {}

unsigned long long __fc_atomic_load(void *obj, size_t obj_size) {
  if (obj_size == sizeof(char)) return *((volatile atomic_uchar *)obj);
  else if (obj_size == sizeof(short)) return *((volatile atomic_ushort *)obj);
  else if (obj_size == sizeof(int)) return *((volatile atomic_uint *)obj);
  else if (obj_size == sizeof(long)) return *((volatile atomic_ulong *)obj);
  else if (obj_size == sizeof(long long))
    return *((volatile atomic_ullong *)obj);
  else assert(0);
  return 0;
}

unsigned long long __fc_atomic_load_explicit(void *object,
                                             memory_order order,
                                             size_t obj_size) {
  return __fc_atomic_load(object, obj_size);
}

unsigned long long __fc_atomic_exchange(void *obj,
                                        unsigned long long desired,
                                        size_t obj_size) {
  unsigned long long r = 0;
  if (obj_size == sizeof(char)) {
    r = *((volatile atomic_uchar *)obj);
    *((volatile atomic_uchar *)obj) = desired;
  } else if (obj_size == sizeof(short)) {
    r =  *((volatile atomic_ushort *)obj);
    *((volatile atomic_ushort *)obj) = desired;
  } else if (obj_size == sizeof(int)) {
    r =  *((volatile atomic_uint *)obj);
    *((volatile atomic_uint *)obj) = desired;
  } else if (obj_size == sizeof(long)) {
    r = *((volatile atomic_ulong *)obj);
    *((volatile atomic_ulong *)obj) = desired;
  } else if (obj_size == sizeof(long long)) {
    r = *((volatile atomic_ullong *)obj);
    *((volatile atomic_ullong *)obj) = desired;
  } else assert(0);
  return r;
}

unsigned long long __fc_atomic_exchange_explicit(void *object,
                                                 unsigned long long desired,
                                                 memory_order order,
                                                 size_t obj_size) {
  return __fc_atomic_exchange(object, desired, obj_size);
}

_Bool __fc_atomic_compare_exchange_strong(void *object, void *expected,
                                          unsigned long long desired,
                                          size_t obj_size) {
  int r = memcmp(object, expected, obj_size);
  if (r == 0) memcpy(object, &desired, obj_size);
  else memcpy(expected, object, obj_size);
  return r == 0;
}

_Bool __fc_atomic_compare_exchange_strong_explicit(void *object, void *expected,
                                                   unsigned long long desired,
                                                   memory_order success,
                                                   memory_order failure,
                                                   size_t obj_size) {
  return __fc_atomic_compare_exchange_strong(object, expected, desired, obj_size);
}

_Bool __fc_atomic_compare_exchange_weak(void *object, void *expected,
                                        unsigned long long desired,
                                        size_t obj_size) {
  return __fc_atomic_compare_exchange_strong(object, expected, desired, obj_size);
}

_Bool __fc_atomic_compare_exchange_weak_explicit(void *object, void *expected,
                                                 unsigned long long desired,
                                                 memory_order success,
                                                 memory_order failure,
                                                 size_t obj_size) {
  return __fc_atomic_compare_exchange_strong(object, expected, desired, obj_size);
}

unsigned long long __fc_atomic_fetch_add(void *obj, unsigned long long operand,
                                         size_t obj_size) {
  unsigned long long r = 0;
  if (obj_size == sizeof(char)) {
    r = *((volatile atomic_uchar *)obj);
    *((volatile atomic_uchar *)obj) += operand;
  } else if (obj_size == sizeof(short)) {
    r =  *((volatile atomic_ushort *)obj);
    *((volatile atomic_ushort *)obj) += operand;
  } else if (obj_size == sizeof(int)) {
    r =  *((volatile atomic_uint *)obj);
    *((volatile atomic_uint *)obj) += operand;
  } else if (obj_size == sizeof(long)) {
    r = *((volatile atomic_ulong *)obj);
    *((volatile atomic_ulong *)obj) += operand;
  } else if (obj_size == sizeof(long long)) {
    r = *((volatile atomic_ullong *)obj);
    *((volatile atomic_ullong *)obj) += operand;
  } else assert(0);
  return r;
}

unsigned long long __fc_atomic_fetch_add_explicit(void *obj,
                                                  unsigned long long operand,
                                                  memory_order order,
                                                  size_t obj_size) {
  return __fc_atomic_fetch_add(obj, operand, obj_size);
}

unsigned long long __fc_atomic_fetch_sub(void *obj, unsigned long long operand,
                                         size_t obj_size) {
  unsigned long long r = 0;
  if (obj_size == sizeof(char)) { r = *((volatile atomic_uchar *)obj);
    *((volatile atomic_uchar *)obj) -= operand; }
  else if (obj_size == sizeof(short)) { r =  *((volatile atomic_ushort *)obj);
    *((volatile atomic_ushort *)obj) -= operand; }
  else if (obj_size == sizeof(int)) { r =  *((volatile atomic_uint *)obj);
    *((volatile atomic_uint *)obj) -= operand; }
  else if (obj_size == sizeof(long)) { r = *((volatile atomic_ulong *)obj);
    *((volatile atomic_ulong *)obj) -= operand; }
  else if (obj_size == sizeof(long long)) { r = *((volatile atomic_ullong *)obj);
    *((volatile atomic_ullong *)obj) -= operand; }
  else assert(0);
  return r;
}

unsigned long long __fc_atomic_fetch_sub_explicit(void *obj,
                                                  unsigned long long operand,
                                                  memory_order order,
                                                  size_t obj_size) {
  return __fc_atomic_fetch_sub(obj, operand, obj_size);
}

unsigned long long __fc_atomic_fetch_or(void *obj, unsigned long long operand,
                                        size_t obj_size) {
  unsigned long long r = 0;
  if (obj_size == sizeof(char)) { r = *((volatile atomic_uchar *)obj);
    *((volatile atomic_uchar *)obj) |= operand; }
  else if (obj_size == sizeof(short)) { r =  *((volatile atomic_ushort *)obj);
    *((volatile atomic_ushort *)obj) |= operand; }
  else if (obj_size == sizeof(int)) { r =  *((volatile atomic_uint *)obj);
    *((volatile atomic_uint *)obj) |= operand; }
  else if (obj_size == sizeof(long)) { r = *((volatile atomic_ulong *)obj);
    *((volatile atomic_ulong *)obj) |= operand; }
  else if (obj_size == sizeof(long long)) { r = *((volatile atomic_ullong *)obj);
    *((volatile atomic_ullong *)obj) |= operand; }
  else assert(0);
  return r;
}

unsigned long long __fc_atomic_fetch_or_explicit(void *obj,
                                                 unsigned long long operand,
                                                 memory_order order,
                                                 size_t obj_size) {
  return __fc_atomic_fetch_or(obj, operand, obj_size);
}

unsigned long long __fc_atomic_fetch_xor(void *obj, unsigned long long operand,
                                         size_t obj_size) {
  unsigned long long r = 0;
  if (obj_size == sizeof(char)) {
    r = *((volatile atomic_uchar *)obj);
    *((volatile atomic_uchar *)obj) ^= operand;
  } else if (obj_size == sizeof(short)) {
    r =  *((volatile atomic_ushort *)obj);
    *((volatile atomic_ushort *)obj) ^= operand;
  } else if (obj_size == sizeof(int)) {
    r =  *((volatile atomic_uint *)obj);
    *((volatile atomic_uint *)obj) ^= operand;
  } else if (obj_size == sizeof(long)) {
    r = *((volatile atomic_ulong *)obj);
    *((volatile atomic_ulong *)obj) ^= operand;
  } else if (obj_size == sizeof(long long)) {
    r = *((volatile atomic_ullong *)obj);
    *((volatile atomic_ullong *)obj) ^= operand;
  }
  else assert(0);
  return r;
}

unsigned long long __fc_atomic_fetch_xor_explicit(void *obj,
                                                  unsigned long long operand,
                                                  memory_order order,
                                                  size_t obj_size) {
  return __fc_atomic_fetch_xor(obj, operand, obj_size);
}

unsigned long long __fc_atomic_fetch_and(void *obj,
                                         unsigned long long operand,
                                         size_t obj_size) {
  unsigned long long r = 0;
  if (obj_size == sizeof(char)) {
    r = *((volatile atomic_uchar *)obj);
    *((volatile atomic_uchar *)obj) &= operand;
  } else if (obj_size == sizeof(short)) {
    r =  *((volatile atomic_ushort *)obj);
    *((volatile atomic_ushort *)obj) &= operand;
  } else if (obj_size == sizeof(int)) {
    r =  *((volatile atomic_uint *)obj);
    *((volatile atomic_uint *)obj) &= operand;
  } else if (obj_size == sizeof(long)) {
    r = *((volatile atomic_ulong *)obj);
    *((volatile atomic_ulong *)obj) &= operand;
  } else if (obj_size == sizeof(long long)) {
    r = *((volatile atomic_ullong *)obj);
    *((volatile atomic_ullong *)obj) &= operand;
  } else assert(0);
  return r;
}

unsigned long long __fc_atomic_fetch_and_explicit(void *obj,
                                                  unsigned long long operand,
                                                  memory_order order,
                                                  size_t obj_size) {
  return __fc_atomic_fetch_and(obj, operand, obj_size);
}

_Bool atomic_flag_test_and_set(volatile atomic_flag *object) {
  _Bool r = object->__fc_val;
  object->__fc_val = 1;
  return r;
}

_Bool atomic_flag_test_and_set_explicit(volatile atomic_flag *object,
                                        memory_order order) {
  _Bool r = object->__fc_val;
  object->__fc_val = 1;
  return r;
}

void atomic_flag_clear(volatile atomic_flag *object) {
  object->__fc_val = 0;
}

void atomic_flag_clear_explicit(volatile atomic_flag *object,
                                memory_order order) {
  object->__fc_val = 0;
}

__POP_FC_STDLIB

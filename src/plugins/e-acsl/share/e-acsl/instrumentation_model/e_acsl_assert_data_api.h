/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/*! ***********************************************************************
 * \file
 * \brief E-ACSL utility functions for assertions.
 **************************************************************************/

#ifndef E_ACSL_ASSERT_DATA_API_H
#define E_ACSL_ASSERT_DATA_API_H

#include "../internals/e_acsl_alias.h"
#include "../numerical_model/e_acsl_gmp_api.h"
#include "e_acsl_assert_data.h"

#ifdef __FC_FEATURES_H
#  include <__fc_alloc_axiomatic.h>
#else
/*@ ghost extern int __fc_heap_status; */
#endif

// Use an array of arbitrary length to serve as dynamic allocation base in the
// specifications so that Eva can better interpret contracts in this file.
//@ ghost static eacsl_assert_data_value_t __fc_eacsl_assert_data_value_base[INT32_MAX];

#define eacsl_assert_register_bool      export_alias(assert_register_bool)
#define eacsl_assert_register_char      export_alias(assert_register_char)
#define eacsl_assert_register_schar     export_alias(assert_register_schar)
#define eacsl_assert_register_uchar     export_alias(assert_register_uchar)
#define eacsl_assert_register_int       export_alias(assert_register_int)
#define eacsl_assert_register_uint      export_alias(assert_register_uint)
#define eacsl_assert_register_short     export_alias(assert_register_short)
#define eacsl_assert_register_ushort    export_alias(assert_register_ushort)
#define eacsl_assert_register_long      export_alias(assert_register_long)
#define eacsl_assert_register_ulong     export_alias(assert_register_ulong)
#define eacsl_assert_register_longlong  export_alias(assert_register_longlong)
#define eacsl_assert_register_ulonglong export_alias(assert_register_ulonglong)
#define eacsl_assert_register_mpz       export_alias(assert_register_mpz)
#define eacsl_assert_register_float     export_alias(assert_register_float)
#define eacsl_assert_register_double    export_alias(assert_register_double)
#define eacsl_assert_register_longdouble                                       \
  export_alias(assert_register_longdouble)
#define eacsl_assert_register_mpq    export_alias(assert_register_mpq)
#define eacsl_assert_register_ptr    export_alias(assert_register_ptr)
#define eacsl_assert_register_array  export_alias(assert_register_array)
#define eacsl_assert_register_fun    export_alias(assert_register_fun)
#define eacsl_assert_register_struct export_alias(assert_register_struct)
#define eacsl_assert_register_union  export_alias(assert_register_union)
#define eacsl_assert_register_other  export_alias(assert_register_other)
#define eacsl_assert_copy_values     export_alias(assert_copy_values)
#define eacsl_assert_clean           export_alias(assert_clean)

/************************************************************************/
/*** Register integers {{{ ***/
/************************************************************************/

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_bool(eacsl_assert_data_t *data, const char *name,
                                int is_enum, _Bool value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_char(eacsl_assert_data_t *data, const char *name,
                                int is_enum, char value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_schar(eacsl_assert_data_t *data, const char *name,
                                 int is_enum, signed char value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_uchar(eacsl_assert_data_t *data, const char *name,
                                 int is_enum, unsigned char value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_int(eacsl_assert_data_t *data, const char *name,
                               int is_enum, int value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_uint(eacsl_assert_data_t *data, const char *name,
                                int is_enum, unsigned int value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_short(eacsl_assert_data_t *data, const char *name,
                                 int is_enum, short value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_ushort(eacsl_assert_data_t *data, const char *name,
                                  int is_enum, unsigned short value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_long(eacsl_assert_data_t *data, const char *name,
                                int is_enum, long value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_ulong(eacsl_assert_data_t *data, const char *name,
                                 int is_enum, unsigned long value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_longlong(eacsl_assert_data_t *data, const char *name,
                                    int is_enum, long long value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_ulonglong(eacsl_assert_data_t *data,
                                     const char *name, int is_enum,
                                     unsigned long long value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:is_enum, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_mpz(eacsl_assert_data_t *data, const char *name,
                               int is_enum, const eacsl_mpz_t value)
    __attribute__((FC_BUILTIN));

/* }}} */

/************************************************************************/
/*** Register reals {{{ ***/
/************************************************************************/

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_float(eacsl_assert_data_t *data, const char *name,
                                 float value) __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_double(eacsl_assert_data_t *data, const char *name,
                                  double value) __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_longdouble(eacsl_assert_data_t *data,
                                      const char *name, long double value)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:value;
  @ ensures \valid(data->values); */
void eacsl_assert_register_mpq(eacsl_assert_data_t *data, const char *name,
                               const eacsl_mpq_t value)
    __attribute__((FC_BUILTIN));

/* }}} */

/************************************************************************/
/*** Register pointers {{{ ***/
/************************************************************************/

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:ptr;
  @ ensures \valid(data->values); */
void eacsl_assert_register_ptr(eacsl_assert_data_t *data, const char *name,
                               void *ptr) __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name, indirect:array;
  @ ensures \valid(data->values); */
void eacsl_assert_register_array(eacsl_assert_data_t *data, const char *name,
                                 void *array) __attribute__((FC_BUILTIN));

/* }}} */

/************************************************************************/
/*** Register composite {{{ ***/
/************************************************************************/

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name;
  @ ensures \valid(data->values); */
void eacsl_assert_register_fun(eacsl_assert_data_t *data, const char *name)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name;
  @ ensures \valid(data->values); */
void eacsl_assert_register_struct(eacsl_assert_data_t *data, const char *name)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name;
  @ ensures \valid(data->values); */
void eacsl_assert_register_union(eacsl_assert_data_t *data, const char *name)
    __attribute__((FC_BUILTIN));

/* }}} */

/************************************************************************/
/*** Register other types {{{ ***/
/************************************************************************/

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ allocates data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from &__fc_eacsl_assert_data_value_base,
                               indirect:name;
  @ ensures \valid(data->values); */
void eacsl_assert_register_other(eacsl_assert_data_t *data, const char *name)
    __attribute__((FC_BUILTIN));

/* }}} */

/************************************************************************/
/*** Miscellaneous functions {{{ ***/
/************************************************************************/

/*@ requires \valid(dest) && \valid(src);
  @ requires dest->values == \null || \valid(dest->values);
  @ requires src->values == \null || \valid(src->values);
  @ allocates dest->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns dest->values \from &__fc_eacsl_assert_data_value_base;
  @ ensures dest->values == \null || \valid(dest->values); */
void eacsl_assert_copy_values(eacsl_assert_data_t *dest,
                              eacsl_assert_data_t *src)
    __attribute__((FC_BUILTIN));

/*@ requires \valid(data);
  @ requires data->values == \null || \valid(data->values);
  @ frees data->values;
  @ assigns __fc_heap_status \from __fc_heap_status;
  @ assigns data->values \from \nothing;
  @ ensures data->values == \null; */
void eacsl_assert_clean(eacsl_assert_data_t *data) __attribute__((FC_BUILTIN));

/* }}} */

#endif // E_ACSL_ASSERT_DATA_API_H

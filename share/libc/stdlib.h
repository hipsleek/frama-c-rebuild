/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/* ISO C: 7.20 */
#ifndef __FC_STDLIB_H
#define __FC_STDLIB_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_machdep.h"
#include "__fc_define_size_t.h"
#include "__fc_define_sys_wait_macros.h"
#include "__fc_define_wchar_t.h"
#include "__fc_alloc_axiomatic.h"
#include "__fc_string_axiomatic.h"
#include <errno.h>
#include <stddef.h>
__BEGIN_DECLS

#ifndef __div_t_defined
typedef struct __fc_div_t {
  int quot;              /* Quotient.  */
  int rem;               /* Remainder.  */
} div_t;
#define __div_t_defined 1
#endif

#ifndef __ldiv_t_defined
typedef struct __fc_ldiv_t {
  long int quot;              /* Quotient.  */
  long int rem;               /* Remainder.  */
} ldiv_t;
#define __ldiv_t_defined 1
#endif

#ifndef __lldiv_t_defined
typedef struct __fc_lldiv_t {
  long long int quot;              /* Quotient.  */
  long long int rem;               /* Remainder.  */
} lldiv_t;
#define __lldiv_t_defined 1
#endif

#include "__fc_define_null.h"

/* These could be customizable */
#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0

#include <limits.h>

#define RAND_MAX __FC_RAND_MAX
#define MB_CUR_MAX __FC_MB_CUR_MAX

/*@
  requires valid_nptr: valid_read_string(nptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
 */
extern double atof(const char *nptr);

/*@
  requires valid_nptr: valid_read_string(nptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
 */
extern int atoi(const char *nptr);

/*@
  requires valid_nptr: valid_read_string(nptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
 */
extern long int atol(const char *nptr);

/*@
  requires valid_nptr: valid_read_string(nptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
 */
extern long long int atoll(const char *nptr);

/* See ISO C: 7.20.1.3 to complete these specifications */
/*@
  requires valid_string_nptr: valid_read_string(nptr);
  requires separation: \separated(nptr, endptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
  assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                        indirect:endptr;
  behavior no_storage:
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
  behavior store_position:
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
    assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:endptr;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
  complete behaviors;
  disjoint behaviors;
*/
extern double strtod(const char * restrict nptr,
     char ** restrict endptr);

/*@
  requires valid_string_nptr: valid_read_string(nptr);
  requires separation: \separated(nptr, endptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
  assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                        indirect:endptr;
  behavior no_storage:
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
  behavior store_position:
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
    assigns *endptr \from nptr, indirect:nptr[0 ..], indirect:endptr;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
  complete behaviors;
  disjoint behaviors;
*/
extern float strtof(const char * restrict nptr,
     char ** restrict endptr);

/*@
  requires valid_string_nptr: valid_read_string(nptr);
  requires separation: \separated(nptr, endptr);
  assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
  assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                        indirect:endptr;
  behavior no_storage:
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
  behavior store_position:
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)];
    assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:endptr;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
  complete behaviors;
  disjoint behaviors;
*/
extern long double strtold(const char * restrict nptr,
     char ** restrict endptr);

#include "__fc_strto_axiomatic.h"

/*@
  requires valid_string_nptr: valid_read_string(nptr);
  requires separation: \separated(nptr, endptr);
  requires base_range: base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                        indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                        indirect:endptr, indirect:base;
  assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
  behavior cannot_convert:
    assumes
      no_conversion: str_to_integer(nptr, LONG_MIN, LONG_MAX, base) == 0;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
    ensures result_no_conversion: \result == 0;
    ensures errno_set: __fc_errno == EINVAL;
  behavior out_of_range_null_endptr:
    assumes
      out_of_range: str_to_integer(nptr, LONG_MIN, LONG_MAX, base) == 2;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
    ensures result_out_of_range: \result == LONG_MIN || \result == LONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior out_of_range_nonnull_endptr:
    // Note: the standard does not state that endptr is definitively assigned
    //       to in this case, so we assume it may be.
    assumes
      out_of_range: str_to_integer(nptr, LONG_MIN, LONG_MAX, base) == 2;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
    assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
    ensures result_out_of_range: \result == LONG_MIN || \result == LONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior in_range_null_endptr:
    assumes in_range: str_to_integer(nptr, LONG_MIN, LONG_MAX, base) == 1;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
  behavior in_range_nonnull_endptr:
    assumes in_range: str_to_integer(nptr, LONG_MIN, LONG_MAX, base) == 1;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
  complete behaviors;
  disjoint behaviors;
*/
extern long int strtol(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);

/*@
  requires valid_string_nptr: valid_read_string(nptr);
  requires separation: \separated(nptr, endptr);
  requires base_range: base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                        indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                        indirect:endptr, indirect:base;
  assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
  behavior cannot_convert:
    assumes
      no_conversion: str_to_integer(nptr, LLONG_MIN, LLONG_MAX, base) == 0;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
    ensures result_no_conversion: \result == 0;
    ensures errno_set: __fc_errno == EINVAL;
  behavior out_of_range_null_endptr:
    assumes
      out_of_range: str_to_integer(nptr, LLONG_MIN, LLONG_MAX, base) == 2;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
    ensures result_out_of_range: \result == LLONG_MIN || \result == LLONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior out_of_range_nonnull_endptr:
    // Note: the standard does not state that endptr is definitively assigned
    //       to in this case, so we assume it may be.
    assumes
      out_of_range: str_to_integer(nptr, LLONG_MIN, LLONG_MAX, base) == 2;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
    assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
    ensures result_out_of_range: \result == LLONG_MIN || \result == LLONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior in_range_null_endptr:
    assumes in_range: str_to_integer(nptr, LLONG_MIN, LLONG_MAX, base) == 1;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
  behavior in_range_nonnull_endptr:
    assumes in_range: str_to_integer(nptr, LLONG_MIN, LLONG_MAX, base) == 1;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
  complete behaviors;
  disjoint behaviors;
*/
extern long long int strtoll(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);

/*@
  requires valid_string_nptr: valid_read_string(nptr);
  requires separation: \separated(nptr, endptr);
  requires base_range: base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                        indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                        indirect:endptr, indirect:base;
  assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
  behavior cannot_convert:
    assumes
      no_conversion: str_to_integer(nptr, 0, ULONG_MAX, base) == 0;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
    ensures result_no_conversion: \result == 0;
    ensures errno_set: __fc_errno == EINVAL;
  behavior out_of_range_null_endptr:
    assumes
      out_of_range: str_to_integer(nptr, 0, ULONG_MAX, base) == 2;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
    ensures result_out_of_range: \result == ULONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior out_of_range_nonnull_endptr:
    // Note: the standard does not state that endptr is definitively assigned
    //       to in this case, so we assume it may be.
    assumes
      out_of_range: str_to_integer(nptr, 0, ULONG_MAX, base) == 2;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
    assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
    ensures result_out_of_range: \result == ULONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior in_range_null_endptr:
    assumes in_range: str_to_integer(nptr, 0, ULONG_MAX, base) == 1;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
  behavior in_range_nonnull_endptr:
    assumes in_range: str_to_integer(nptr, 0, ULONG_MAX, base) == 1;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
  complete behaviors;
  disjoint behaviors;
*/
extern unsigned long int strtoul(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);

/*@
  requires valid_string_nptr: valid_read_string(nptr);
  requires separation: \separated(nptr, endptr);
  requires base_range: base == 0 || 2 <= base <= 36;
  assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                        indirect:base;
  assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                        indirect:endptr, indirect:base;
  assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
  behavior cannot_convert:
    assumes
      no_conversion: str_to_integer(nptr, 0, ULLONG_MAX, base) == 0;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
    ensures result_no_conversion: \result == 0;
    ensures errno_set: __fc_errno == EINVAL;
  behavior out_of_range_null_endptr:
    assumes
      out_of_range: str_to_integer(nptr, 0, ULLONG_MAX, base) == 2;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
    ensures result_out_of_range: \result == ULLONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior out_of_range_nonnull_endptr:
    // Note: the standard does not state that endptr is definitively assigned
    //       to in this case, so we assume it may be.
    assumes
      out_of_range: str_to_integer(nptr, 0, ULLONG_MAX, base) == 2;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
    assigns __fc_errno \from indirect:nptr[0 .. strlen(nptr)], indirect:base;
    ensures result_out_of_range: \result == ULLONG_MAX;
    ensures errno_set: __fc_errno == ERANGE;
  behavior in_range_null_endptr:
    assumes in_range: str_to_integer(nptr, 0, ULLONG_MAX, base) == 1;
    assumes null_endptr: endptr == \null;
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
  behavior in_range_nonnull_endptr:
    assumes in_range: str_to_integer(nptr, 0, ULLONG_MAX, base) == 1;
    assumes nonnull_endptr: endptr != \null;
    requires valid_endptr: \valid(endptr);
    assigns \result \from indirect:nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:base;
    assigns *endptr \from nptr, indirect:nptr[0 .. strlen(nptr)],
                          indirect:endptr, indirect:base;
    ensures initialization: \initialized(endptr);
    ensures valid_endptr_content: \valid_read(*endptr);
    ensures endptr_same_base: \base_addr(*endptr) == \base_addr(nptr);
  complete behaviors;
  disjoint behaviors;
*/
extern unsigned long long int strtoull(
     const char * restrict nptr,
     char ** restrict endptr,
     int base);

//@ ghost __FC_EXTERN int __fc_random_counter __attribute__((unused));
//@ ghost __FC_INTERN const unsigned long __fc_rand_max = __FC_RAND_MAX;

/* ISO C: 7.20.2 */
/*@
  assigns \result \from indirect:__fc_random_counter;
  assigns __fc_random_counter \from __fc_random_counter;
  ensures result_range: 0 <= \result <= __fc_rand_max;
*/
extern int rand(void);

/*@ assigns __fc_random_counter \from seed ; */
extern void srand(unsigned int seed);

/*@
  assigns \result \from indirect:__fc_random_counter;
  ensures result_range: 0 <= \result <= __fc_rand_max;
*/
extern long int random(void);

/*@ assigns __fc_random_counter \from seed; */
extern void srandom(unsigned int seed);

// used to check if some *48() functions have called the seed initializer
//@ ghost __FC_INTERN int __fc_random48_init;

__FC_EXTERN unsigned short __fc_random48_counter[3];

/*@
  assigns __fc_random48_counter[0..2] \from seed;
  assigns __fc_random48_init \from \nothing;
  ensures random48_initialized: __fc_random48_init == 1;
*/
extern void srand48 (long int seed);

/*@
  requires valid_seed16v: \valid(seed16v+(0..2));
  requires initialization:initialized_seed16v: \initialized(seed16v+(0..2));
  assigns __fc_random48_counter[0..2] \from seed16v[0..2];
  assigns __fc_random48_init \from \nothing;
  assigns \result \from &__fc_random48_counter;
  ensures random48_initialized: __fc_random48_init == 1;
  ensures result_counter: \result == &__fc_random48_counter[0];
*/
extern unsigned short *seed48(unsigned short seed16v[3]);

/*@
  requires valid_param: \valid(param+(0..6));
  requires initialization:initialized_param: \initialized(param+(0..6));
  assigns __fc_random48_counter[0..2] \from param[0..6];
  assigns __fc_random48_init \from \nothing;
  ensures random48_initialized: __fc_random48_init == 1;
*/
extern void lcong48(unsigned short param[7]);

/*@
  requires random48_initialized: __fc_random48_init == 1;
  assigns __fc_random48_counter[0..2] \from __fc_random48_counter[0..2];
  assigns \result \from __fc_random48_counter[0..2];
  ensures result_range: \is_finite(\result) && 0.0 <= \result < 1.0;
*/
extern double drand48(void);

/*@
  requires valid_xsubi: \valid(xsubi+(0..2));
  requires initialization:initialized_xsubi: \initialized(xsubi+(0..2));
  assigns __fc_random48_counter[0..2] \from __fc_random48_counter[0..2];
  assigns \result \from __fc_random48_counter[0..2];
  ensures result_range: \is_finite(\result) && 0.0 <= \result < 1.0;
*/
extern double erand48(unsigned short xsubi[3]);

/*@
  requires random48_initialized: __fc_random48_init == 1;
  assigns __fc_random48_counter[0..2] \from __fc_random48_counter[0..2];
  assigns \result \from __fc_random48_counter[0..2];
  ensures result_range: 0 <= \result < 2147483648;
*/
extern long int lrand48 (void);

/*@
  requires valid_xsubi: \valid(xsubi+(0..2));
  requires initialization:initialized_xsubi: \initialized(xsubi+(0..2));
  assigns __fc_random48_counter[0..2] \from __fc_random48_counter[0..2];
  assigns \result \from __fc_random48_counter[0..2];
  ensures result_range: 0 <= \result < 2147483648;
*/
extern long int nrand48 (unsigned short xsubi[3]);

/*@
  requires random48_initialized: __fc_random48_init == 1;
  assigns __fc_random48_counter[0..2] \from __fc_random48_counter[0..2];
  assigns \result \from __fc_random48_counter[0..2];
  ensures result_range: -2147483648 <= \result < 2147483648;
*/
extern long int mrand48 (void);

/*@
  requires valid_xsubi: \valid(xsubi+(0..2));
  requires initialization:initialized_xsubi: \initialized(xsubi+(0..2));
  assigns __fc_random48_counter[0..2] \from __fc_random48_counter[0..2];
  assigns \result \from __fc_random48_counter[0..2];
  ensures result_range: -2147483648 <= \result < 2147483648;
*/
extern long int jrand48 (unsigned short xsubi[3]);

/*@ predicate is_alignment(integer n) =
      n > 0 && ((size_t)n & ((size_t)n - 1)) == 0;
*/
/*@ predicate is_valid_alignment(integer n) =
      is_alignment(n) && n < __FC_IMPLEM_MAX_ALIGN;
*/

/*@
#if __STDC_VERSION__ == 201112L
    requires valid_alignment: is_valid_alignment(alignment);
    requires alignment_modulo: size % alignment == 0;
  #endif
    allocates \result;
    assigns __fc_heap_status \from size, __fc_heap_status;
    assigns \result, __fc_errno \from indirect:size, indirect:__fc_heap_status;
    behavior allocation:
      #if __STDC__VERSION == 201112L
      assumes valid_alignment: is_valid_alignment(alignment);
      #endif
      assumes can_allocate: is_allocable(size);
      #if __STDC__VERSION != 201112L
      assumes valid_alignment: is_valid_alignment(alignment);
      #endif
      assigns __fc_heap_status \from size, __fc_heap_status;
      assigns \result \from indirect:size, indirect:__fc_heap_status;
      ensures allocation: \fresh(\result,size);
      ensures alignment: \aligned(\result, alignment);
      ensures errno_same: __fc_errno == \old(__fc_errno);
    behavior no_allocation:
      assumes cannot_allocate: !is_allocable(size);
      #if __STDC_VERSION != 201112L
      assumes valid_alignment: is_valid_alignment(alignment);
      #endif
      allocates \nothing;
      assigns \result \from \nothing;
      ensures null_result: \result==\null;
      ensures errno_set: __fc_errno == ENOMEM;
    #if __STDC_VERSION != 201112L
    behavior no_allocation_invalid_align:
      assumes cannot_allocate: !is_valid_alignment(alignment);
      allocates \nothing;
      assigns \result \from \nothing;
      ensures null_result: \result==\null;
      ensures errno_set: __fc_errno == EINVAL;
    #endif
    complete behaviors;
    disjoint behaviors;
*/
extern void* aligned_alloc (size_t alignment, size_t size);

/* ISO C: 7.20.3.1 */
/*@
  allocates \result;
  assigns __fc_heap_status \from indirect:nmemb, indirect:size, __fc_heap_status;
  assigns \result, __fc_errno \from indirect:nmemb, indirect:size,
                                    indirect:__fc_heap_status;

  behavior allocation:
    assumes can_allocate: is_allocable(nmemb * size);
    ensures allocation: \fresh(\result, nmemb * size);
    ensures alignment: \aligned(\result, alignof(max_align_t));
    ensures initialization: \initialized(((char *)\result)+(0..nmemb*size-1));
    ensures zero_initialization: \subset(((char *)\result)[0..nmemb*size-1], {0});
    ensures errno_same: __fc_errno == \old(__fc_errno);

  behavior no_allocation:
    assumes cannot_allocate: !is_allocable(nmemb * size);
    allocates \nothing;
    assigns \result \from \nothing;
    ensures null_result: \result == \null;
    ensures errno_set: __fc_errno == ENOMEM;

  complete behaviors;
  disjoint behaviors; */
extern void *calloc(size_t nmemb, size_t size);

/*@ allocates \result;
  @ assigns __fc_heap_status \from size, __fc_heap_status;
  @ assigns \result, __fc_errno \from indirect:size, indirect:__fc_heap_status;
  @ behavior allocation:
  @   assumes can_allocate: is_allocable(size);
  @   assigns __fc_heap_status \from size, __fc_heap_status;
  @   assigns \result \from indirect:size, indirect:__fc_heap_status;
  @   ensures allocation: \fresh(\result,size);
  @   ensures alignment: \aligned(\result, alignof(max_align_t));
  @   ensures errno_same: __fc_errno == \old(__fc_errno);
  @ behavior no_allocation:
  @   assumes cannot_allocate: !is_allocable(size);
  @   allocates \nothing;
  @   assigns \result \from \nothing;
  @   ensures null_result: \result==\null;
  @   ensures errno_set: __fc_errno == ENOMEM;
  @ complete behaviors;
  @ disjoint behaviors;
  @*/
extern void *malloc(size_t size);

/*@ requires freeable: p==\null || \freeable(p);
  @ frees p;
  @ assigns  __fc_heap_status \from __fc_heap_status;
  @ behavior deallocation:
  @   assumes  nonnull_p: p!=\null;
  @   assigns  __fc_heap_status \from __fc_heap_status;
  @   ensures  freed: \allocable(p);
  @ behavior no_deallocation:
  @   assumes  null_p: p==\null;
  @   frees    \nothing;
  @   assigns  \nothing;
  @ complete behaviors;
  @ disjoint behaviors;
  @*/
extern void free(void *p);

/*@
   requires freeable: ptr == \null || \freeable(ptr);
   allocates \result;
   frees     ptr;
   assigns   __fc_heap_status \from __fc_heap_status;
   assigns   \result \from size, ptr, __fc_heap_status;
   assigns   __fc_errno \from indirect:size, indirect:__fc_heap_status;

   behavior allocation:
     assumes   can_allocate: is_allocable(size);
     allocates \result;
     assigns   \result \from size, __fc_heap_status;
     ensures   allocation: \fresh(\result,size);
     ensures   alignment: \aligned(\result, alignof(max_align_t));
     ensures   errno_same: __fc_errno == \old(__fc_errno);

   behavior deallocation:
     assumes   nonnull_ptr: ptr != \null;
     assumes   can_allocate: is_allocable(size);
     frees     ptr;
     ensures   freed: \allocable(ptr);
     ensures   freeable: \result == \null || \freeable(\result);
     ensures   errno_same: __fc_errno == \old(__fc_errno);

   behavior fail:
     assumes cannot_allocate: !is_allocable(size);
     allocates \nothing;
     frees     \nothing;
     assigns   \result \from size, __fc_heap_status;
     ensures   null_result: \result == \null;
     ensures   errno_set: __fc_errno == ENOMEM;

   complete behaviors;
   disjoint behaviors allocation, fail;
   disjoint behaviors deallocation, fail;
  */
extern void *realloc(void *ptr, size_t size);

// reallocarray is non-POSIX (glibc)
/*@
   requires freeable: ptr == \null || \freeable(ptr);
   allocates \result;
   frees     ptr;
   assigns   __fc_heap_status \from indirect:nmemb, indirect:size,
                                    __fc_heap_status;
   assigns   \result \from ptr, indirect:nmemb, indirect:size,
                           indirect:__fc_heap_status;
   assigns   __fc_errno \from indirect:size, indirect: nmemb,
                              indirect:__fc_heap_status;

   behavior allocation:
     assumes   can_allocate: is_allocable(nmemb * size);
     allocates \result;
     ensures   allocation: \fresh(\result, nmemb * size);
     ensures   errno_same: __fc_errno == \old(__fc_errno);

   behavior deallocation:
     assumes   nonnull_ptr: ptr != \null;
     assumes   can_allocate: is_allocable(nmemb * size);
     frees     ptr;
     ensures   freed: \allocable(ptr);
     ensures   freeable: \result == \null || \freeable(\result);
     ensures   errno_same: __fc_errno == \old(__fc_errno);

   behavior fail:
     assumes cannot_allocate: !is_allocable(nmemb * size);
     allocates \nothing;
     frees     \nothing;
     assigns   \result \from indirect:nmemb, indirect:size,
                             indirect:__fc_heap_status;
     ensures   null_result: \result == \null;
     ensures   errno_set: __fc_errno == ENOMEM;

   complete behaviors;
   disjoint behaviors allocation, fail;
   disjoint behaviors deallocation, fail;
  */
extern void *reallocarray(void *ptr, size_t nmemb, size_t size);

/* ISO C: 7.20.4 */

/*@
  terminates \false;
  assigns \exit_status \from \nothing;
  exits status: \exit_status != EXIT_SUCCESS;
  ensures never_terminates: \false;
 */
extern void abort(void) __attribute__ ((__noreturn__));

/*@ assigns \result \from \nothing ;*/
extern int atexit(void (*func)(void));

/*@ assigns \result \from \nothing ;*/
extern int at_quick_exit(void (*func)(void));

/*@
  terminates \false;
  assigns \exit_status \from status;
  exits status: \exit_status == status;
  ensures never_terminates: \false;
*/
extern void exit(int status) __attribute__ ((__noreturn__));

/*@
  terminates \false;
  assigns \nothing;
  ensures never_terminates: \false;
*/
extern void _Exit(int status) __attribute__ ((__noreturn__));

__FC_EXTERN char *__fc_env[ARG_MAX];

/*@
  requires valid_name: valid_read_string(name);
  assigns \result \from __fc_env[0..], indirect:name,
                        indirect:name[0 .. strlen(name)];
  ensures null_or_valid_result:
    \result == \null || (\valid(\result) && valid_read_string(\result));
 */
extern char *getenv(const char *name);

// Non-POSIX, GNU extension
/*@
  requires valid_name: valid_read_string(name);
  assigns \result \from __fc_env[0..], indirect:name,
                        indirect:name[0 .. strlen(name)];
  ensures null_or_valid_result:
    \result == \null || (\valid(\result) && valid_read_string(\result));
 */
extern char *secure_getenv(const char *name);

/*@
  requires valid_string: valid_read_string(string);
  assigns __fc_env[0..] \from __fc_env[0..], string;
  assigns \result \from indirect:__fc_env[0..], indirect:string;
*/
extern int putenv(char *string);

/*@
  requires valid_name: valid_read_string(name);
  requires valid_value: valid_read_string(value);
  allocates __fc_env[0..];
  assigns \result, __fc_env[0..], __fc_errno
    \from __fc_env[0..], indirect:name[0 .. strlen(name)],
    indirect:value[0 .. strlen(value)], indirect:overwrite;
  assigns __fc_env[0..][0..] \from name[0 .. strlen(name)],
                                   value[0 .. strlen(value)],
                                   indirect:__fc_env[0..],
                                   indirect:overwrite;
  ensures result_ok_or_error: \result == 0 || \result == -1;
  behavior invalid_name:
    assumes name_empty_or_with_equals_sign:
      strlen(name) == 0 || strchr(name, '=');
    allocates \nothing;
    assigns \result \from indirect:name[0 .. strlen(name)];
    assigns __fc_errno \from indirect:name[0 .. strlen(name)];
    ensures result_error: \result == -1;
    ensures errno_set: __fc_errno == EINVAL;
  behavior out_of_memory:
    assumes not_enough_memory: !is_allocable(strlen(name) + strlen(value) + 2);
    allocates \nothing;
    assigns \result, __fc_errno \from indirect:name[0 .. strlen(name)],
                                      indirect:value[0 .. strlen(value)],
                                      indirect:overwrite;
    ensures result_error: \result == -1;
    ensures errno_set: __fc_errno == ENOMEM;
  behavior valid_name:
    assumes name_not_empty: strlen(name) > 0;
    assumes name_has_no_equals_sign: !strchr(name, '=');
    assumes enough_memory: is_allocable(strlen(name) + strlen(value) + 2);
    allocates __fc_env[0..];
    assigns \result, __fc_env[0..]
      \from __fc_env[0..], indirect:name[0 .. strlen(name)],
            indirect:value[0 .. strlen(value)], indirect:overwrite;
    assigns __fc_env[0..][0..] \from name[0 .. strlen(name)],
                                     value[0 .. strlen(value)],
                                     indirect:__fc_env[0..],
                                     indirect:overwrite;
    ensures result_zero: \result == 0;
*/
extern int setenv(const char *name, const char *value, int overwrite);

/*@
  requires valid_name: valid_read_string(name);
  assigns \result, __fc_env[0..]
    \from __fc_env[0..], indirect:name, indirect:name[0 .. strlen(name)];
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int unsetenv(const char *name);

/*@
  terminates \false;
  assigns \nothing;
  ensures never_terminates: \false;
 */
extern void quick_exit(int status) __attribute__ ((__noreturn__));

/*@
  requires null_or_valid_string_command:
     command == \null || valid_read_string(command);
  assigns \result \from indirect:command,
                        indirect:command[0 .. strlen(command)];
*/
extern int system(const char *command);

/* ISO C: 7.20.5 */

/* TODO: use one of the well known specification with high order compare :-) */
// NOTE: the assigns of function [compar] are not currently taken into account
// by ACSL. If [compar] is not purely functional, the result may be unsound.
// To ensure soundness, you should manually give a specification to the
// comparison function that is equivalent to:
//     assigns \result \from *(<type>*)a, *(<type>*)b;
// where <type> is the type of the compared arguments.
/*@
  requires valid_function_compar: \valid_function(compar);
  assigns \result \from indirect:key, ((char*)key)[0 .. size-1], base,
                        ((char*)base)[0 .. size * (nmemb-1)], indirect:nmemb,
                        indirect:size, indirect:*compar;
  ensures null_or_correct_result: \result == \null ||
          \subset(\result, (void*)(((char*)base) + (0 .. size * (nmemb-1))));
*/
extern void *bsearch(const void *key, const void *base, size_t nmemb,
                     size_t size, int (*compar)(const void *, const void *));

// NOTE: the assigns of function [compar] are not currently taken into account
// by ACSL. If [compar] is not purely functional, the result may be unsound.
/*@
  requires valid_function_compar: \valid_function(compar);
  assigns ((char*)base)[0 ..] \from indirect:base, ((char*)base)[0 ..],
                                    indirect:nmemb, indirect:size,
                                    indirect:compar, indirect:*compar;
 */
extern void qsort(void *base, size_t nmemb, size_t size,
             int (*compar)(const void *, const void *));

/* ISO C: 7.20.6 */

/*@
  requires abs_representable: j > INT_MIN;
  assigns \result \from j;
  behavior negative:
    assumes negative: j < 0;
    ensures opposite_result: \result == -j;
  behavior nonnegative:
    assumes nonnegative: j >= 0;
    ensures same_result: \result == j;
  complete behaviors;
  disjoint behaviors;
 */
extern int abs(int j);

/*@
  requires abs_representable: j > LONG_MIN ;
  assigns \result \from j;
  behavior negative:
    assumes negative: j < 0;
    ensures opposite_result: \result == -j;
  behavior nonnegative:
    assumes nonnegative: j >= 0;
    ensures same_result: \result == j;
  complete behaviors;
  disjoint behaviors;
 */
extern long int labs(long int j);

/*@
  requires abs_representable: j > LLONG_MIN ;
  assigns \result \from j;
  behavior negative:
    assumes negative: j < 0;
    ensures opposite_result: \result == -j;
  behavior nonnegative:
    assumes nonnegative: j >= 0;
    ensures same_result: \result == j;
  complete behaviors;
  disjoint behaviors;
 */
extern long long int llabs(long long int j);

/*@
  requires denom_nonzero: denom != 0;
  requires no_overflow: !(numer == INT_MIN && denom == -1);
  assigns \result \from numer, denom;
*/
extern div_t div(int numer, int denom);

/*@
  requires denom_nonzero: denom != 0;
  requires no_overflow: !(numer == LONG_MIN && denom == -1);
  assigns \result \from numer, denom;
*/
extern ldiv_t ldiv(long int numer, long int denom);

/*@
  requires denom_nonzero: denom != 0;
  requires no_overflow: !(numer == LLONG_MIN && denom == -1);
  assigns \result \from numer, denom;
*/
extern lldiv_t lldiv(long long int numer, long long int denom);

/* ISO C: 7.20.7 */

//@ ghost __FC_EXTERN int __fc_mblen_state;

/*@ assigns \result, __fc_mblen_state \from
    indirect:s, indirect:s[0 ..], indirect:n, __fc_mblen_state; */
extern int mblen(const char *s, size_t n);

//@ ghost __FC_EXTERN int __fc_mbtowc_state;

/*@
  requires separation: \separated(pwc, s);
  assigns \result \from indirect:s, indirect:s[0 .. n-1], indirect:n,
                        __fc_mbtowc_state;
  assigns pwc[0 .. \result-1], __fc_mbtowc_state
    \from indirect:s, s[0 .. n-1], indirect:n, __fc_mbtowc_state;
  ensures consumed_range: \result <= n;
*/
extern int mbtowc(wchar_t * restrict pwc,
     const char * restrict s,
     size_t n);

//@ ghost __FC_EXTERN int __fc_wctomb_state;

/*@
  //requires room_string: \valid(s + (0 .. __fc_mb_cur_max - 1));
  assigns \result \from indirect:wc, __fc_wctomb_state;
  assigns s[0 ..], __fc_wctomb_state \from wc, __fc_wctomb_state;
*/
extern int wctomb(char *s, wchar_t wc);

/* ISO C: 7.20.8 */

/*@
  requires separation: \separated(pwcs, s);
  assigns \result \from indirect:s, indirect:s[0 .. n-1], indirect:n;
  assigns pwcs[0 .. n-1] \from indirect:s, s[0 .. n-1], indirect:n;
*/
extern size_t mbstowcs(wchar_t * restrict pwcs,
     const char * restrict s,
     size_t n);

/*@
  requires separation: \separated(s, pwcs);
  assigns \result \from indirect:pwcs, indirect:pwcs[0 .. n-1], indirect:n;
  assigns s[0 .. n-1] \from indirect:pwcs, pwcs[0 .. n-1], indirect:n;
*/
extern size_t wcstombs(char * restrict s,
     const wchar_t * restrict pwcs,
     size_t n);

/*@
  // Returns true iff n is a suitable alignment according to POSIX, i.e.,
  // a power of two multiple of sizeof(void *).
  predicate is_posix_alignment(integer n) =
     is_alignment(n) && n % sizeof(void*) == 0 ;
*/

// Note: this specification should ideally use a more specific predicate,
//       such as 'is_allocable_aligned(alignment, size)'.
/*@
  requires valid_memptr: \valid(memptr);
  allocates *memptr;
  assigns __fc_heap_status \from indirect:alignment, size, __fc_heap_status;
  assigns \result \from indirect:alignment, indirect:size,
                        indirect:__fc_heap_status;
  behavior invalid_alignment:
    assumes alignment_not_suitable: !is_posix_alignment(alignment);
    assigns \result \from indirect:alignment;
    ensures result_einval: \result == EINVAL;
  behavior allocation:
    assumes alignment_is_suitable: is_posix_alignment(alignment);
    assumes can_allocate: is_allocable(size);
    assigns __fc_heap_status \from indirect:alignment, size, __fc_heap_status;
    assigns \result \from indirect:alignment, indirect:size,
                          indirect:__fc_heap_status;
    ensures allocation: \fresh(*memptr,size);
    ensures result_zero: \result == 0;
  behavior no_allocation:
    assumes alignment_is_suitable: is_posix_alignment(alignment);
    assumes cannot_allocate: !is_allocable(size);
    allocates \nothing;
    assigns \result \from indirect:alignment;
    ensures result_non_zero: \result < 0 || \result > 0;
  complete behaviors;
  disjoint behaviors;
 */
extern int posix_memalign(void **memptr, size_t alignment, size_t size);

/*@
  // missing: requires 'last 6 characters of template must be XXXXXX'
  // missing: assigns \result, templat[0..] \from 'filesystem', 'RNG';
  requires valid_template: valid_string(templat);
  requires template_len: strlen(templat) >= 6;
  assigns templat[0..] \from \nothing;
  assigns \result \from \nothing;
  ensures result_error_or_valid_fd: \result == -1 ||
                                    0 <= \result < __FC_FOPEN_MAX;
 */
extern int mkstemp(char *templat);

/*@
  // missing: requires 'last 6 characters of template must be XXXXXX'
  // missing: assigns \result, templat[0..] \from 'filesystem', 'RNG';
  // missing: flags == O_APPEND || O_CLOEXEC || O_SYNC
  requires valid_template: valid_string(templat);
  requires template_len: strlen(templat) >= 6;
  assigns templat[0..] \from \nothing;
  assigns \result \from \nothing;
  ensures result_error_or_valid_fd: \result == -1 ||
                                    0 <= \result < __FC_FOPEN_MAX;
 */
extern int mkostemp(char *templat, int flags);

/*@
  // missing: requires 'last (6+suffixlen) characters of template must be X's'
  // missing: assigns \result, templat[0..] \from 'filesystem', 'RNG';
  requires valid_template: valid_string(templat);
  requires template_len: strlen(templat) >= 6 + suffixlen;
  requires non_negative_suffixlen: suffixlen >= 0;
  assigns templat[0..] \from \nothing;
  assigns \result \from \nothing;
  ensures result_error_or_valid_fd: \result == -1 ||
                                    0 <= \result < __FC_FOPEN_MAX;
 */
extern int mkstemps(char *templat, int suffixlen);

/*@
  // missing: requires 'last (6+suffixlen) characters of template must be X's'
  // missing: assigns \result, templat[0..] \from 'filesystem', 'RNG';
  // missing: flags == O_APPEND || O_CLOEXEC || O_SYNC
  requires valid_template: valid_string(templat);
  requires template_len: strlen(templat) >= 6 + suffixlen;
  requires non_negative_suffixlen: suffixlen >= 0;
  assigns templat[0..] \from \nothing;
  assigns \result \from \nothing;
  ensures result_error_or_valid_fd: \result == -1 ||
                                    0 <= \result < __FC_FOPEN_MAX;
 */
extern int mkostemps(char *templat, int suffixlen, int flags);

// 'realpath' may allocate memory for the result, which is not supported by
// some plugins such as Eva. In such cases, it is preferable to use the stub
// provided in stdlib.c.
/*@
  requires valid_file_name_or_null:
    file_name == \null || valid_read_string(file_name);
  requires resolved_name_null_or_allocated:
    resolved_name == \null || \valid(resolved_name+(0 .. PATH_MAX-1));
  assigns __fc_heap_status \from indirect:resolved_name, __fc_heap_status;
  assigns \result[0 .. PATH_MAX-1], resolved_name[0 .. PATH_MAX-1] \from
    indirect:__fc_heap_status, indirect:resolved_name, indirect:file_name[0..];
  assigns __fc_errno \from
    indirect:file_name, indirect:file_name[0..], indirect:resolved_name,
    indirect:__fc_heap_status;
  behavior null_file_name:
    assumes null_file_name: file_name == \null;
    assigns \result, __fc_errno \from indirect:file_name;
    ensures null_result: \result == \null;
    ensures errno_set: __fc_errno == EINVAL;
  behavior allocate_resolved_name:
    assumes valid_file_name: valid_read_string(file_name);
    assumes resolved_name_null: resolved_name == \null;
    assumes can_allocate: is_allocable(PATH_MAX);
    assigns __fc_heap_status \from __fc_heap_status;
    assigns \result \from indirect:__fc_heap_status;
    ensures allocation: \fresh(\result,PATH_MAX);
  behavior not_enough_memory:
    assumes valid_file_name: valid_read_string(file_name);
    assumes resolved_name_null: resolved_name == \null;
    assumes cannot_allocate: !is_allocable(PATH_MAX);
    allocates \nothing;
    assigns \result \from \nothing;
    ensures null_result: \result == \null;
    ensures errno_set: __fc_errno == ENOMEM;
  behavior resolved_name_buffer:
    assumes valid_file_name: valid_read_string(file_name);
    assumes allocated_resolved_name_or_fail:
      \valid(resolved_name+(0 .. PATH_MAX-1));
    allocates \nothing;
    assigns \result \from resolved_name;
    assigns resolved_name[0 .. PATH_MAX-1] \from indirect:file_name[0..];
    ensures valid_string_resolved_name: valid_string(resolved_name)
                                        && strlen(resolved_name) < PATH_MAX;
    // missing: assigns \result,
    //                  resolved_name[0 .. PATH_MAX-1] \from 'filesystem';
    ensures resolved_result: \result == resolved_name;
  behavior filesystem_error:
    assumes valid_file_name: valid_read_string(file_name);
    allocates \nothing;
    assigns \result, __fc_errno
      \from indirect:file_name[0..]; // missing: indirect:'filesystem';
    ensures null_result: \result == \null;
    ensures errno_set:
      __fc_errno \in {EACCES, EIO, ELOOP, ENAMETOOLONG, ENOENT, ENOTDIR};
*/
extern char *realpath(const char *restrict file_name,
                      char *restrict resolved_name);

// Non-POSIX; GNU extension
// This function may allocate memory for the result, which is not supported by
// some plugins such as Eva. In such cases, it is preferable to use the stub
// provided in stdlib.c.
/*@
  allocates \result;
  assigns \result \from path[0 .. strlen(path)];
 */
extern char *canonicalize_file_name(const char *path);

extern volatile int Frama_C_entropy_source;

// Non-POSIX
/*@
  requires valid_p: \valid(((char*)buf) + (0 .. n-1));
  assigns ((char*)buf)[0 .. n-1] \from Frama_C_entropy_source;
  assigns Frama_C_entropy_source \from Frama_C_entropy_source;
  ensures initialization: \initialized(((char*)buf) + (0 .. n-1));
*/
void arc4random_buf(void *buf, size_t n);

__END_DECLS

__POP_FC_STDLIB
#endif

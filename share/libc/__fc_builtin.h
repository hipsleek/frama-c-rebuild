/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_BUILTIN_H
#define __FC_BUILTIN_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_alloc_axiomatic.h"
#include "__fc_define_wint_t.h"
#include <stddef.h>
#include <stdint.h>

__BEGIN_DECLS

__FC_EXTERN volatile int Frama_C_entropy_source __attribute__((unused));

/*@ requires valid_p: \valid(p + (0 .. l-1));
    assigns p[0 .. l-1] \from Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures initialization: \initialized(p + (0 .. l-1));
*/
extern void Frama_C_make_unknown(char *p, size_t l) __attribute__((FC_BUILTIN));

/*@ requires valid_p: \valid(p + (0 .. l-1));
    assigns p[0 .. l-1] \from Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures initialization: \initialized(p + (0 .. l-1));
*/
extern void Frama_C_make_unknown_wchar(wchar_t *p, size_t l) __attribute__((FC_BUILTIN));

/*@ assigns \result \from a, b, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_a_or_b: \result == a || \result == b ;
 */
extern int Frama_C_nondet(int a, int b) __attribute__((FC_BUILTIN));

/*@ assigns \result \from a, b, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_a_or_b: \result == a || \result == b ;
 */
extern void *Frama_C_nondet_ptr(void *a, void *b) __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern int Frama_C_interval(int min, int max) __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern int Frama_C_interval_split(int min, int max) __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern unsigned char Frama_C_unsigned_char_interval(unsigned char min,
                                                    unsigned char max)
    __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern char Frama_C_char_interval(char min, char max)
    __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern unsigned short Frama_C_unsigned_short_interval(unsigned short min,
                                                      unsigned short max)
    __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern short Frama_C_short_interval(short min, short max)
    __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern unsigned int Frama_C_unsigned_int_interval(unsigned int min,
                                                  unsigned int max)
    __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern int Frama_C_int_interval(int min, int max) __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern unsigned long Frama_C_unsigned_long_interval(unsigned long min,
                                                    unsigned long max)
    __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern long Frama_C_long_interval(long min, long max)
    __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern unsigned long long Frama_C_unsigned_long_long_interval(
    unsigned long long min, unsigned long long max) __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern long long Frama_C_long_long_interval(long long min, long long max)
    __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern size_t Frama_C_size_t_interval(size_t min, size_t max)
    __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern intmax_t Frama_C_intmax_t_interval(intmax_t min, intmax_t max)
    __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern uintmax_t Frama_C_uintmax_t_interval(uintmax_t min, uintmax_t max)
    __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern ptrdiff_t Frama_C_ptrdiff_t_interval(ptrdiff_t min, ptrdiff_t max)
    __attribute__((FC_BUILTIN));

/*@ requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: min <= \result <= max ;
 */
extern wint_t Frama_C_wint_t_interval(wint_t min, wint_t max)
    __attribute__((FC_BUILTIN));

/*@
  assigns \result \from min, max, Frama_C_entropy_source;
  assigns Frama_C_entropy_source \from Frama_C_entropy_source;
  behavior finite:
    assumes finite: \is_finite(min) && \is_finite(max);
    requires order: min <= max;
    ensures result_bounded: \is_finite(\result) && min <= \result <= max;
  behavior infinite_not_nan:
    assumes infinite: \is_infinite(min) || \is_infinite(max);
    assumes not_nan: !\is_NaN(min) && !\is_NaN(max);
    requires order: min <= max;
    ensures result_bounded: !\is_NaN(\result) && min <= \result <= max;
  disjoint behaviors;
*/
extern float Frama_C_float_interval(float min, float max)
    __attribute__((FC_BUILTIN));

/*@
  assigns \result \from min, max, Frama_C_entropy_source;
  assigns Frama_C_entropy_source \from Frama_C_entropy_source;
  behavior finite:
    assumes finite: \is_finite(min) && \is_finite(max);
    requires order: min <= max;
    ensures result_bounded: \is_finite(\result) && min <= \result <= max;
  behavior infinite_not_nan:
    assumes infinite: \is_infinite(min) || \is_infinite(max);
    assumes not_nan: !\is_NaN(min) && !\is_NaN(max);
    requires order: min <= max;
    ensures result_bounded: !\is_NaN(\result) && min <= \result <= max;
  disjoint behaviors;
*/
extern double Frama_C_double_interval(double min, double max)
    __attribute__((FC_BUILTIN));

/*@ requires finite: \is_finite(min) && \is_finite(max);
    requires order: min <= max;
    assigns \result \from min, max, Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    ensures result_bounded: \is_finite(\result) && min <= \result <= max;
 */
extern double Frama_C_real_interval_as_double(double min, double max)
    __attribute__((FC_BUILTIN));

/*@ // Signals an error;
  terminates \false;
  assigns \nothing;
  ensures never_terminates: \false;
*/
extern void Frama_C_abort(void) __attribute__((__noreturn__))
__attribute__((FC_BUILTIN));

/*@ assigns \result \from p; */
extern size_t Frama_C_offset(const void *p) __attribute__((FC_BUILTIN));

/*@
  allocates \result;
  assigns __fc_heap_status \from size, __fc_heap_status;
  assigns \result \from indirect:size, indirect:__fc_heap_status;
*/
extern void *Frama_C_malloc_fresh(size_t size) __attribute__((FC_BUILTIN));

//@ assigns \result \from i;
extern long long Frama_C_abstract_cardinal(long long i)
    __attribute__((FC_BUILTIN));

//@ assigns \result \from i;
extern long long Frama_C_abstract_max(long long i) __attribute__((FC_BUILTIN));

//@ assigns \result \from i;
extern long long Frama_C_abstract_min(long long i) __attribute__((FC_BUILTIN));

//@ assigns \nothing;
extern void Frama_C_watch_value(void *p, size_t size, int i, int n)
    __attribute__((FC_BUILTIN));

//@ assigns \nothing;
extern void Frama_C_watch_cardinal(void *p, size_t size, int i, int n)
    __attribute__((FC_BUILTIN));

__END_DECLS

__POP_FC_STDLIB
#endif

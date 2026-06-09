/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/* ISO C: 7.12 */
#ifndef __FC_MATH_H
#define __FC_MATH_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_string_axiomatic.h"
#include <errno.h>
__BEGIN_DECLS

typedef float float_t;
typedef double double_t;
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202311L
typedef _Float32 _Float32_t;
typedef _Float64 _Float64_t;
#endif

#define MATH_ERRNO	1
#define MATH_ERREXCEPT	2

/* The constants below are not part of C99/C11 but they are defined in POSIX */
#define M_E 0x1.5bf0a8b145769p1         /* e          */
#define M_LOG2E 0x1.71547652b82fep0     /* log_2 e    */
#define M_LOG10E 0x1.bcb7b1526e50ep-2   /* log_10 e   */
#define M_LN2 0x1.62e42fefa39efp-1      /* log_e 2    */
#define M_LN10 0x1.26bb1bbb55516p1      /* log_e 10   */
#define M_PI 0x1.921fb54442d18p1        /* pi         */
#define M_PI_2 0x1.921fb54442d18p0      /* pi/2       */
#define M_PI_4 0x1.921fb54442d18p-1     /* pi/4       */
#define M_1_PI 0x1.45f306dc9c883p-2     /* 1/pi       */
#define M_2_PI 0x1.45f306dc9c883p-1     /* 2/pi       */
#define M_2_SQRTPI 0x1.20dd750429b6dp0  /* 2/sqrt(pi) */
#define M_SQRT2 0x1.6a09e667f3bcdp0     /* sqrt(2)    */
#define M_SQRT1_2 0x1.6a09e667f3bcdp-1  /* 1/sqrt(2)  */

/* The following specifications will set errno. */
#define math_errhandling	MATH_ERRNO

#define FP_NAN 0
#define FP_INFINITE 1
#define FP_ZERO 2
#define FP_SUBNORMAL 3
#define FP_NORMAL 4

#define FP_ILOGB0 __FC_INT_MIN
#define FP_ILOGBNAN __FC_INT_MIN

#include <float.h> // for DBL_MIN and FLT_MIN

/*@
  assigns \result \from x;
  behavior nan_argument:
    assumes is_nan: \is_NaN(x);
    ensures fp_nan: \result == FP_NAN;
  behavior infinite_argument:
    assumes is_infinite: !\is_NaN(x) && !\is_finite(x);
    ensures fp_infinite: \result == FP_INFINITE;
  behavior zero_argument:
    assumes is_a_zero: x == 0.0; // also includes -0.0
    ensures fp_zero: \result == FP_ZERO;
  behavior subnormal_argument:
    assumes is_finite: \is_finite(x);
    assumes is_subnormal: (x > 0.0 && x < FLT_MIN) || (x < 0.0 && x > -FLT_MIN);
    ensures fp_subnormal: \result == FP_SUBNORMAL;
  behavior normal_argument:
    assumes is_finite: \is_finite(x);
    assumes not_subnormal: (x <= -FLT_MIN || x >= FLT_MIN);
    ensures fp_normal: \result == FP_NORMAL;
  complete behaviors;
  disjoint behaviors;
 */
__FC_EXTERN_FOR_MACRO(fpclassify) int __fc_fpclassifyf(float x);

/*@
  assigns \result \from x;
  behavior nan_argument:
    assumes is_nan: \is_NaN(x);
    ensures fp_nan: \result == FP_NAN;
  behavior infinite_argument:
    assumes is_infinite: !\is_NaN(x) && !\is_finite(x);
    ensures fp_infinite: \result == FP_INFINITE;
  behavior zero_argument:
    assumes is_a_zero: x == 0.0; // also includes -0.0
    ensures fp_zero: \result == FP_ZERO;
  behavior subnormal_argument:
    assumes is_finite: \is_finite(x);
    assumes is_subnormal: (x > 0.0 && x < DBL_MIN) || (x < 0.0 && x > -DBL_MIN);
    ensures fp_subnormal: \result == FP_SUBNORMAL;
  behavior normal_argument:
    assumes is_finite: \is_finite(x);
    assumes not_subnormal: (x <= -DBL_MIN || x >= DBL_MIN);
    ensures fp_normal: \result == FP_NORMAL;
  complete behaviors;
  disjoint behaviors;
 */
__FC_EXTERN_FOR_MACRO(fpclassify) int __fc_fpclassify(double x);

/*@
  assigns \result \from x;
  behavior nan_argument:
    assumes is_nan: \is_NaN(x);
    ensures fp_nan: \result == FP_NAN;
  behavior infinite_argument:
    assumes is_infinite: !\is_NaN(x) && !\is_finite(x);
    ensures fp_infinite: \result == FP_INFINITE;
  behavior zero_argument:
    assumes is_a_zero: x == 0.0; // also includes -0.0
    ensures fp_zero: \result == FP_ZERO;
  behavior subnormal_argument:
    assumes is_finite: \is_finite(x);
    assumes is_subnormal: (x > 0.0 && x < LDBL_MIN) || (x < 0.0 && x > -LDBL_MIN);
    ensures fp_subnormal: \result == FP_SUBNORMAL;
  behavior normal_argument:
    assumes is_finite: \is_finite(x);
    assumes not_subnormal: (x <= -LDBL_MIN || x >= LDBL_MIN);
    ensures fp_normal: \result == FP_NORMAL;
  complete behaviors;
  disjoint behaviors;
*/
__FC_EXTERN_FOR_MACRO(fpclassify) int __fc_fpclassifyl(long double x);

#define fpclassify(x) _Generic(x,                                       \
                               float: __fc_fpclassifyf(x),              \
                               double: __fc_fpclassify(x),              \
                               long double: __fc_fpclassifyl(x))

#define isnan(x) (fpclassify(x) == FP_NAN)

#define isnormal(x) _Generic(x,                                         \
                             float: __fc_fpclassifyf(x) == FP_NORMAL, \
                             double: __fc_fpclassify(x) == FP_NORMAL, \
                             long double: __fc_fpclassifyl(x) == FP_NORMAL)


/* Note: for the isinf builtin, GCC returns 1 for +inf and -1 for -inf, so we
   cannot simply apply fpclassify(x) == FP_INFINITE.
*/

/*@
  assigns \result \from x;
  behavior pos_infinity_argument:
    assumes is_plus_infinity: \is_plus_infinity(x);
    ensures res_one: \result == 1;
  behavior neg_infinity_argument:
    assumes is_minus_infinity: \is_minus_infinity(x);
    ensures res_minus_one: \result == -1;
  behavior finite_argument:
    assumes not_infinity: !\is_infinite (x);
    ensures res_zero: \result == 0;
  complete behaviors;
  disjoint behaviors;
 */
__FC_EXTERN_FOR_MACRO(isinf) int __fc_isinff(float x);

/*@
  assigns \result \from x;
  behavior pos_infinity_argument:
    assumes is_plus_infinity: \is_plus_infinity(x);
    ensures res_one: \result == 1;
  behavior neg_infinity_argument:
    assumes is_minus_infinity: \is_minus_infinity(x);
    ensures res_minus_one: \result == -1;
  behavior finite_argument:
    assumes not_infinity: !\is_infinite (x);
    ensures res_zero: \result == 0;
  complete behaviors;
  disjoint behaviors;
 */
__FC_EXTERN_FOR_MACRO(isinf) int __fc_isinf(double x);

/*@
  assigns \result \from x;
  behavior pos_infinity_argument:
    assumes is_plus_infinity: \is_plus_infinity(x);
    ensures res_one: \result == 1;
  behavior neg_infinity_argument:
    assumes is_minus_infinity: \is_minus_infinity(x);
    ensures res_minus_one: \result == -1;
  behavior finite_argument:
    assumes not_infinity: !\is_infinite (x);
    ensures res_zero: \result == 0;
  complete behaviors;
  disjoint behaviors;
 */
__FC_EXTERN_FOR_MACRO(isinf) int __fc_isinfl(long double x);

#define isinf(x) _Generic(x,                           \
                           float: __fc_isinff(x),       \
                           double: __fc_isinf(x),       \
                           long double: __fc_isinfl(x))

/*@
  assigns \result \from x;
  ensures res_nonzero_if_negative: \sign(x) == \Negative <==> \result != 0;
*/
__FC_EXTERN_FOR_MACRO(signbit) int __fc_signbitf(float x);

/*@
  assigns \result \from x;
  ensures res_nonzero_if_negative: \sign(x) == \Negative <==> \result != 0;
*/
__FC_EXTERN_FOR_MACRO(signbit) int __fc_signbit(double x);

/*@
  assigns \result \from x;
  ensures res_nonzero_if_negative: \sign(x) == \Negative <==> \result != 0;
*/
__FC_EXTERN_FOR_MACRO(signbit) int __fc_signbitl(long double x);

#define signbit(x) _Generic(x,                                  \
                            float: __fc_signbitf(x),            \
                            double: __fc_signbit(x),            \
                            long double: __fc_signbitl(x))


/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double acos(double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float acosf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double acosl(long double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double asin(double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float asinf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double asinl(long double x);

/*@
  assigns \result \from x;
  behavior normal_argument:
    assumes number_arg: !\is_NaN(x);
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1.571 <= \result <= 1.571;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float atanf(float x);

/*@
  assigns \result \from x;
  behavior normal_argument:
    assumes number_arg: !\is_NaN(x);
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1.571 <= \result <= 1.571;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double atan(double x);

/*@
  assigns \result \from x;
  behavior normal_argument:
    assumes number_arg: !\is_NaN(x);
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1.571 <= \result <= 1.571;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double atanl(long double x);

/*@
  assigns \result \from x, y;
  behavior normal_argument:
    assumes number_args: !\is_NaN(x) && !\is_NaN(y);
    ensures finite_result: \is_finite(\result);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x) || \is_NaN(y);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double atan2(double y, double x);

/*@
  assigns \result \from x, y;
  behavior normal_argument:
    assumes number_args: !\is_NaN(x) && !\is_NaN(y);
    ensures finite_result: \is_finite(\result);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x) || \is_NaN(y);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float atan2f(float y, float x);

/*@
  assigns \result \from x, y;
  behavior normal_argument:
    assumes number_args: !\is_NaN(x) && !\is_NaN(y);
    ensures finite_result: \is_finite(\result);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x) || \is_NaN(y);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double atan2l(long double y, long double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double cos(double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float cosf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double cosl(long double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double sin(double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float sinf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double sinl(long double x);

/* Note: the specs of tan/tanf below assume that, for a finite x,
 *       the result is always finite. This is _not_ guaranteed by the standard,
 *       but testing with the GNU libc, plus some mathematical arguments
 *       (see https://stackoverflow.com/questions/67482420) indicate that,
 *       in practice, the result is _never_ infinite.
 *       If you know of any implementations in which a finite argument
 *       produces an infinite result, please inform us.
 */
/*@
  assigns errno, \result \from x;
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    assigns \result \from x;
    ensures zero_res: \is_finite(\result) && \result == x;
    ensures no_error: errno == \old(errno);
  behavior finite_non_zero_argument:
    assumes finite_arg: \is_finite(x) && x != 0.;
    ensures finite_result: \is_finite(\result);
    ensures maybe_error: errno == \old(errno) || errno == ERANGE;
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double tan(double x);

/*@
  assigns errno, \result \from x;
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    assigns \result \from x;
    ensures zero_res: \is_finite(\result) && \result == x;
    ensures no_error: errno == \old(errno);
  behavior finite_non_zero_argument:
    assumes finite_arg: \is_finite(x) && x != 0.;
    ensures finite_result: \is_finite(\result);
    ensures maybe_error: errno == \old(errno) || errno == ERANGE;
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float tanf(float x);

/*@
  assigns errno, \result \from x;
  ensures maybe_error: errno == \old(errno) || errno == EDOM || errno == ERANGE;
*/
extern long double tanl(long double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes in_domain: \is_finite(x) && x >= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes is_plus_infinity: \is_plus_infinity(x);
    assigns \result \from x;
    ensures result_plus_infinity: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_minus_infinity(x) || (\is_finite(x) && x < 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
 */
extern double acosh(double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes in_domain: \is_finite(x) && x >= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes is_plus_infinity: \is_plus_infinity(x);
    assigns \result \from x;
    ensures result_plus_infinity: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_minus_infinity(x) || (\is_finite(x) && x < 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
 */
extern float acoshf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes in_domain: \is_finite(x) && x >= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes is_plus_infinity: \is_plus_infinity(x);
    assigns \result \from x;
    ensures result_plus_infinity: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_minus_infinity(x) || (\is_finite(x) && x < 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
 */
extern long double acoshl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern double asinh(double x);

/*@
  assigns errno, \result \from x;
*/
extern float asinhf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double asinhl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern double atanh(double x);

/*@
  assigns errno, \result \from x;
*/
extern float atanhf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double atanhl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern double cosh(double x);

/*@
  assigns errno, \result \from x;
*/
extern float coshf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double coshl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern double sinh(double x);

/*@
  assigns errno, \result \from x;
*/
extern float sinhf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double sinhl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern double tanh(double x);

/*@
  assigns errno, \result \from x;
*/
extern float tanhf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double tanhl(long double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes domain_arg: x >= -0x1.74910d52d3051p9 && x <= 0x1.62e42fefa39efp+9;
    assigns \result \from x;
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result > 0.;
    ensures no_error: errno == \old(errno);
  behavior overflow_argument:
    assumes overflow_arg: \is_finite(x) && x > 0x1.62e42fefa39efp+9;
    ensures infinite_res: \is_plus_infinity(\result);
    ensures errno_set: errno == ERANGE;
  behavior pos_infinity_argument:
    assumes plus_infinity_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinity_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior underflow_argument:
    assumes underflow_arg: \is_finite(x) && x < -0x1.74910d52d3051p9;
    ensures zero_res: \result == 0.;
    ensures errno_set: errno == ERANGE;
  behavior neg_infinity_argument:
    assumes plus_infinity_arg: \is_minus_infinity(x);
    assigns \result \from x;
    ensures zero_result: \is_finite(\result) && \result == 0.;
    ensures no_error: errno == \old(errno);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double exp(double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes domain_arg: x >= -0x1.9fe368p6 && x <= 0x1.62e42ep+6;
    assigns \result \from x;
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result > 0.;
    ensures no_error: errno == \old(errno);
  behavior overflow_argument:
    assumes overflow_arg: \is_finite(x) && x > 0x1.62e42ep+6;
    ensures infinite_res: \is_plus_infinity(\result);
    ensures errno_set: errno == ERANGE;
  behavior pos_infinity_argument:
    assumes plus_infinity_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinity_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior underflow_argument:
    assumes underflow_arg: \is_finite(x) && x < -0x1.9fe368p6;
    ensures zero_res: \result == 0.;
    ensures errno_set: errno == ERANGE;
  behavior neg_infinity_argument:
    assumes plus_infinity_arg: \is_minus_infinity(x);
    assigns \result \from x;
    ensures zero_result: \is_finite(\result) && \result == 0.;
    ensures no_error: errno == \old(errno);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float expf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double expl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern double exp2(double x);

/*@
  assigns errno, \result \from x;
*/
extern float exp2f(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double exp2l(long double x);

/*@
  assigns errno, \result \from x;
*/
extern double expm1(double x);

/*@
  assigns errno, \result \from x;
*/
extern float expm1f(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double expm1l(long double x);

/*@
  requires valid_exp: \valid(exp);
  assigns \result, *exp \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_nonzero: x != 0.0;
    ensures finite_result: \is_finite(\result);
    ensures bounded_result: 0.5 <= \result < 1.0;
    ensures initialization:exp: \initialized(exp);
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x) || \is_minus_infinity(x);
    ensures infinite_result: \is_infinite(\result);
    ensures result_same_infinite: \result == x;
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures finite_result: \is_finite(\result);
    ensures zero_result: \result == 0.0;
    ensures initialization:exp: \initialized(exp);
    ensures zero_exp: *exp == 0;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double frexp(double x, int *exp);

/*@
  requires valid_exp: \valid(exp);
  assigns \result, *exp \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_nonzero: x != 0.0;
    ensures finite_result: \is_finite(\result);
    ensures bounded_result: 0.5 <= \result < 1.0;
    ensures initialization:exp: \initialized(exp);
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x) || \is_minus_infinity(x);
    ensures infinite_result: \is_infinite(\result);
    ensures result_same_infinite: \result == x;
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures finite_result: \is_finite(\result);
    ensures zero_result: \result == x;
    ensures initialization:exp: \initialized(exp);
    ensures zero_exp: *exp == 0;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float frexpf(float x, int *exp);

/*@
  requires valid_exp: \valid(exp);
  assigns \result, *exp \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_nonzero: x != 0.0;
    ensures finite_result: \is_finite(\result);
    ensures bounded_result: 0.5 <= \result < 1.0;
    ensures initialization:exp: \initialized(exp);
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x) || \is_minus_infinity(x);
    ensures infinite_result: \is_infinite(\result);
    ensures result_same_infinite: \result == x;
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures finite_result: \is_finite(\result);
    ensures zero_result: \result == x;
    ensures initialization:exp: \initialized(exp);
    ensures zero_exp: *exp == 0;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double frexpl(long double x, int *exp);

/*@
  assigns errno, \result \from x;
*/
extern int ilogb(double x);

/*@
  assigns errno, \result \from x;
*/
extern int ilogbf(float x);

/*@
  assigns errno, \result \from x;
*/
extern int ilogbl(long double x);

/*@
  assigns errno, \result \from x, exp;
  behavior normal_argument:
    assumes finite_logic_res: \is_finite((double)(x * pow(2.0d, (double)exp)));
    ensures finite_result: \is_finite(\result);
    ensures errno: errno == ERANGE || errno == \old(errno); //ERANGE if underflow
  behavior overflow_not_nan:
    assumes not_nan_arg: !\is_NaN(x);
    assumes infinite_logic_res: !\is_finite((double)(x * pow(2.0d, (double)exp)));
    ensures infinite_result: \is_infinite(\result);
    ensures errno: errno == ERANGE || errno == \old(errno);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double ldexp(double x, int exp);

/*@
  assigns errno, \result \from x, exp;
  behavior normal_argument:
    assumes finite_logic_res: \is_finite((float)(x * pow(2.0f, (float)exp)));
    ensures finite_result: \is_finite(\result);
    ensures errno: errno == ERANGE || errno == \old(errno); //ERANGE if underflow
  behavior overflow_not_nan:
    assumes not_nan_arg: !\is_NaN(x);
    assumes infinite_logic_res: !\is_finite((float)(x * pow(2.0f, (float)exp)));
    ensures infinite_result: \is_infinite(\result);
    ensures errno: errno == ERANGE || errno == \old(errno);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float ldexpf(float x, int exp);

/*@
  assigns errno, \result \from x, exp;
*/
extern long double ldexpl(long double x, int exp);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VAL
    ensures errno_set: errno == ERANGE;
  behavior negative_argument:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double log(double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VALF
    ensures errno_set: errno == ERANGE;
  behavior negative_argument:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float logf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VALL
    ensures errno_set: errno == ERANGE;
  behavior negative_argument:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double logl(long double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VAL
    ensures errno_set: errno == ERANGE;
  behavior negative_argument:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double log10(double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VALF
    ensures errno_set: errno == ERANGE;
  behavior negative_argument:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float log10f(float x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VALL
    ensures errno_set: errno == ERANGE;
  behavior negative_argument:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double log10l(long double x);

/*@
  assigns errno, \result \from x;
*/
extern double log1p(double x);

/*@
  assigns errno, \result \from x;
*/
extern float log1pf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double log1pl(long double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VAL
    ensures errno_set: errno == ERANGE;
  behavior negative_argument:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double log2(double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VALF
    ensures errno_set: errno == ERANGE;
  behavior negative_argument:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float log2f(float x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero_argument:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VALL
    ensures errno_set: errno == ERANGE;
  behavior negative_argument:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double log2l(long double x);

/*@
  assigns errno, \result \from x;
*/
extern double logb(double x);

/*@
  assigns errno, \result \from x;
*/
extern float logbf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double logbl(long double x);

/*@
  assigns errno, \result, *iptr \from value;
*/
extern double modf(double value, double *iptr);

/*@
  assigns errno, \result, *iptr \from value;
*/
extern float modff(float value, float *iptr);

/*@
  assigns errno, \result, *iptr \from value;
*/
extern long double modfl(long double value, long double *iptr);

/*@
  assigns errno, \result \from x, n;
*/
extern double scalbn(double x, int n);

/*@
  assigns errno, \result \from x, n;
*/
extern float scalbnf(float x, int n);

/*@
  assigns errno, \result \from x, n;
*/
extern long double scalbnl(long double x, int n);

/*@
  assigns errno, \result \from x, n;
*/
extern double scalbln(double x, long int n);

/*@
  assigns errno, \result \from x, n;
*/
extern float scalblnf(float x, long int n);

/*@
  assigns errno, \result \from x, n;
*/
extern long double scalblnl(long double x, long int n);

/*@
  assigns \result \from x;
*/
extern double cbrt(double x);

/*@
  assigns \result \from x;
*/
extern float cbrtf(float x);

/*@
  assigns \result \from x;
*/
extern long double cbrtl(long double x);

/*@
  assigns \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result >= 0.;
    ensures equal_magnitude_result: \result == x || \result == -x;
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_plus_infinity(\result);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double fabs(double x);

/*@
  assigns \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result >= 0.;
    ensures equal_magnitude_result: \result == x || \result == -x;
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_plus_infinity(\result);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float fabsf(float x);

/*@
  assigns \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result >= 0.;
    ensures equal_magnitude_result: \result == x || \result == -x;
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_plus_infinity(\result);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double fabsl(long double x);

/*@
  assigns errno, \result \from x, y;
*/
extern double hypot(double x, double y);

/*@
  assigns errno, \result \from x, y;
*/
extern float hypotf(float x, float y);

/*@
  assigns errno, \result \from x, y;
*/
extern long double hypotl(long double x, long double y);

/*@
  assigns errno, \result \from x, y;
  behavior normal_argument:
    assumes finite_logic_res: \is_finite(pow(x, y));
    ensures finite_result: \is_finite(\result);
    ensures errno: errno == ERANGE || errno == \old(errno);
  behavior overflow_argument:
    assumes infinite_logic_res: !\is_finite(pow(x, y));
    ensures infinite_result: !\is_finite(\result);
    ensures errno: errno == ERANGE || errno == EDOM || errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double pow(double x, double y);

/*@
  assigns errno, \result \from x, y;
  behavior normal_argument:
    assumes finite_logic_res: \is_finite(pow(x, y));
    ensures finite_result: \is_finite(\result);
    ensures errno: errno == ERANGE || errno == \old(errno);
  behavior overflow_argument:
    assumes infinite_logic_res: !\is_finite(pow(x, y));
    ensures infinite_result: !\is_finite(\result);
    ensures errno: errno == ERANGE || errno == EDOM || errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float powf(float x, float y);

/*@
  assigns errno, \result \from x, y;
*/
extern long double powl(long double x, long double y);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x >= -0.;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures positive_result: \result >= -0.;
    ensures result_value: \result == sqrt(x);
    ensures no_error: errno == \old(errno);
  behavior negative_argument:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double sqrt(double x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x >= -0.;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures positive_result: \result >= -0.;
    ensures result_value: \result == sqrt(x);
    ensures no_error: errno == \old(errno);
  behavior negative_argument:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float sqrtf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal_argument:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x >= -0.;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures positive_result: \result >= -0.;
    ensures no_error: errno == \old(errno);
  behavior negative_argument:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior infinite_argument:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double sqrtl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern double erf(double x);

/*@
  assigns errno, \result \from x;
*/
extern float erff(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double erfl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern double erfc(double x);

/*@
  assigns errno, \result \from x;
*/
extern float erfcf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double erfcl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern double lgamma(double x);

/*@
  assigns errno, \result \from x;
*/
extern float lgammaf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double lgammal(long double x);


/*@
  assigns errno, \result \from x;
*/
extern double tgamma(double x);

/*@
  assigns errno, \result \from x;
*/
extern float tgammaf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long double tgammal(long double x);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double ceil(double x);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float ceilf(float x);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double ceill(long double x);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double floor(double x);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float floorf(float x);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double floorl(long double x);

/*@
  assigns \result \from x;
*/
extern double nearbyint(double x);

/*@
  assigns \result \from x;
*/
extern float nearbyintf(float x);

/*@
  assigns \result \from x;
*/
extern long double nearbyintl(long double x);

/*@
  assigns \result \from x;
*/
extern double rint(double x);

/*@
  assigns \result \from x;
*/
extern float rintf(float x);

/*@
  assigns \result \from x;
*/
extern long double rintl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern long int lrint(double x);

/*@
  assigns errno, \result \from x;
*/
extern long int lrintf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long int lrintl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern long long int llrint(double x);

/*@
  assigns errno, \result \from x;
*/
extern long long int llrintf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long long int llrintl(long double x);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double round(double x);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float roundf(float x);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double roundl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern long int lround(double x);

/*@
  assigns errno, \result \from x;
*/
extern long int lroundf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long int lroundl(long double x);

/*@
  assigns errno, \result \from x;
*/
extern long long int llround(double x);

/*@
  assigns errno, \result \from x;
*/
extern long long int llroundf(float x);

/*@
  assigns errno, \result \from x;
*/
extern long long int llroundl(long double x);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double trunc(double x);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float truncf(float x);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite_argument:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan_argument:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double truncl(long double x);

/*@
  assigns errno, \result \from x, y;
  behavior normal_argument:
    assumes in_domain: !\is_NaN(x) && !\is_NaN(y) && y != 0.;
    assigns \result \from x, y;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || y == 0.;
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_args: \is_NaN(x) || \is_NaN(y);
    assigns \result \from x, y;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double fmod(double x, double y);

/*@
  assigns errno, \result \from x, y;
  behavior normal_argument:
    assumes in_domain: !\is_NaN(x) && !\is_NaN(y) && y != 0.;
    assigns \result \from x, y;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || y == 0.;
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan_argument:
    assumes nan_args: \is_NaN(x) || \is_NaN(y);
    assigns \result \from x, y;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float fmodf(float x, float y);

/*@
  assigns errno, \result \from x, y;
*/
extern long double fmodl(long double x, long double y);

/*@
  assigns errno, \result \from x, y;
*/
extern double remainder(double x, double y);

/*@
  assigns errno, \result \from x, y;
*/
extern float remainderf(float x, float y);

/*@
  assigns errno, \result \from x, y;
*/
extern long double remainderl(long double x, long double y);

/*@
  assigns errno, \result, *quo \from x, y;
*/
extern double remquo(double x, double y, int *quo);

/*@
  assigns errno, \result, *quo \from x, y;
*/
extern float remquof(float x, float y, int *quo);

/*@
  assigns errno, \result, *quo \from x, y;
*/
extern long double remquol(long double x, long double y, int *quo);

/*@
  assigns \result \from x, y;
*/
extern double copysign(double x, double y);

/*@
  assigns \result \from x, y;
*/
extern float copysignf(float x, float y);

/*@
  assigns \result \from x, y;
*/
extern long double copysignl(long double x, long double y);

/*@
  requires tagp_valid_string: valid_read_string(tagp);
  assigns \result \from indirect:tagp[0..];
  ensures result_is_nan: \is_NaN(\result);
 */
extern double nan(const char *tagp);

/*@
  requires tagp_valid_string: valid_read_string(tagp);
  assigns \result \from indirect:tagp[0..];
  ensures result_is_nan: \is_NaN(\result);
 */
extern float nanf(const char *tagp);

/*@
  requires tagp_valid_string: valid_read_string(tagp);
  assigns \result \from indirect:tagp[0..];
  ensures result_is_nan: \is_NaN(\result);
 */
extern long double nanl(const char *tagp);

/*@
  assigns errno, \result \from x, y;
*/
extern double nextafter(double x, double y);

/*@
  assigns errno, \result \from x, y;
*/
extern float nextafterf(float x, float y);

/*@
  assigns errno, \result \from x, y;
*/
extern long double nextafterl(long double x, long double y);

/*@
  assigns errno, \result \from x, y;
*/
extern double nexttoward(double x, long double y);

/*@
  assigns errno, \result \from x, y;
*/
extern float nexttowardf(float x, long double y);

/*@
  assigns errno, \result \from x, y;
*/
extern long double nexttowardl(long double x, long double y);

/*@
  assigns errno, \result \from x, y;
*/
extern double fdim(double x, double y);

/*@
  assigns errno, \result \from x, y;
*/
extern float fdimf(float x, float y);

/*@
  assigns errno, \result \from x, y;
*/
extern long double fdiml(long double x, long double y);

/*@
  assigns \result \from x, y;
*/
extern double fmax(double x, double y);

/*@
  assigns \result \from x, y;
*/
extern float fmaxf(float x, float y);

/*@
  assigns \result \from x, y;
*/
extern long double fmaxl(long double x, long double y);

/*@
  assigns \result \from x, y;
*/
extern double fmin(double x, double y);

/*@
  assigns \result \from x, y;
*/
extern float fminf(float x, float y);

/*@
  assigns \result \from x, y;
*/
extern long double fminl(long double x, long double y);

/*@
  assigns errno, \result \from x, y, z;
*/
extern double fma(double x, double y, double z);

/*@
  assigns errno, \result \from x, y, z;
*/
extern float fmaf(float x, float y, float z);

/*@
  assigns errno, \result \from x, y, z;
*/
extern long double fmal(long double x, long double y, long double z);

/*
  Note: __finite, __finitef, __finitel are present in the LSB, e.g.:
  https://refspecs.linuxbase.org/LSB_5.0.0/LSB-Core-generic/LSB-Core-generic/baselib---finitef.html

  Therefore, we keep these names for the functions that are used by the
  isfinite() macro.
*/

/*@
  assigns \result \from f;
  behavior finite_argument:
    assumes isfinite_f: \is_finite(f);
    ensures nonzero_result: \result > 0 || \result < 0;
  behavior nonfinite_argument:
    assumes nonfinite_f: !\is_finite(f);
    ensures zero_result: \result == 0;
  complete behaviors;
  disjoint behaviors;
*/
extern int __finitef(float f);

/*@
  assigns \result \from d;
  behavior finite_argument:
    assumes isfinite_d: \is_finite(d);
    ensures nonzero_result: \result > 0 || \result < 0;
  behavior nonfinite_argument:
    assumes nonfinite_d: !\is_finite(d);
    ensures zero_result: \result == 0;
  complete behaviors;
  disjoint behaviors;
*/
extern int __finite(double d);

/*@
  assigns \result \from x;
  behavior finite_argument:
    assumes isfinite: \is_finite(x);
    ensures nonzero_result: \result > 0 || \result < 0;
  behavior nonfinite_argument:
    assumes nonfinite: !\is_finite(x);
    ensures zero_result: \result == 0;
  complete behaviors;
  disjoint behaviors;
*/
extern int __finitel(long double x);

#define isfinite(x) _Generic(x,                         \
                             float: __finitef(x),       \
                             double: __finite(x),       \
                             long double: __finitel(x))

//The (integer x) argument is just here because a function without argument is
//applied differently in ACSL and C

/*@

  logic float __fc_infinity(integer x) = \plus_infinity;
  logic float __fc_nan(integer x) = \NaN;

*/

/*@
  ensures result_is_infinity: \is_plus_infinity(\result);
  assigns \result \from \nothing;
*/
__FC_EXTERN_FOR_MACRO(INFINITY) float __fc_infinity(int x);

/*@
  ensures result_is_nan: \is_NaN(\result);
  assigns \result \from \nothing;
*/
__FC_EXTERN_FOR_MACRO(NAN) float __fc_nan(int x);


#define INFINITY __fc_infinity(0)
#define NAN __fc_nan(0)

#define HUGE_VALF INFINITY
#define HUGE_VAL  ((double)INFINITY)
#define HUGE_VALL ((long double)INFINITY)


__END_DECLS

__POP_FC_STDLIB
#endif

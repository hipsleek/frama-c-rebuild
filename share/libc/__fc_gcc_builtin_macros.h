/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/*
  This file replaces GCC builtins with calls to the non-builtin functions, to
  reuse their specs.

Usage: add the following to the command line:

    -cpp-extra-args="-include __fc_gcc_builtin_macros.h"

NOTE: this file adds '#include' directives for several libc files, "polluting"
the default namespace. This is the main reason why this file is not included
by default by Frama-C.

This list is based on https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html.

_Complex-related builtins are commented out since Frama-C does not currently
support them.
*/

#ifndef __FC_GCC_BUILTIN_MACROS_H
#define __FC_GCC_BUILTIN_MACROS_H

#include <ctype.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wctype.h>

#define __builtin_abort abort
#define __builtin_abs abs
#define __builtin_acos acos
#define __builtin_acosh acosh
#define __builtin_acoshf acoshf
#define __builtin_acoshl acoshl
#define __builtin_asin asin
#define __builtin_asinh asinh
#define __builtin_asinhf asinhf
#define __builtin_asinhl asinhl
#define __builtin_atan atan
#define __builtin_atan2 atan2
#define __builtin_atanh atanh
#define __builtin_atanhf atanhf
#define __builtin_atanhl atanhl
//#define __builtin_cabs cabs
//#define __builtin_cabsf cabsf
//#define __builtin_cabsl cabsl
//#define __builtin_cacos cacos
//#define __builtin_cacosf cacosf
//#define __builtin_cacosh cacosh
//#define __builtin_cacoshf cacoshf
//#define __builtin_cacoshl cacoshl
//#define __builtin_cacosl cacosl
#define __builtin_calloc calloc
//#define __builtin_carg carg
//#define __builtin_cargf cargf
//#define __builtin_cargl cargl
//#define __builtin_casin casin
//#define __builtin_casinf casinf
//#define __builtin_casinh casinh
//#define __builtin_casinhf casinhf
//#define __builtin_casinhl casinhl
//#define __builtin_casinl casinl
//#define __builtin_catan catan
//#define __builtin_catanf catanf
//#define __builtin_catanh catanh
//#define __builtin_catanhf catanhf
//#define __builtin_catanhl catanhl
//#define __builtin_catanl catanl
//#define __builtin_cbrt cbrt
//#define __builtin_cbrtf cbrtf
//#define __builtin_cbrtl cbrtl
//#define __builtin_ccos ccos
//#define __builtin_ccosf ccosf
//#define __builtin_ccosh ccosh
//#define __builtin_ccoshf ccoshf
//#define __builtin_ccoshl ccoshl
//#define __builtin_ccosl ccosl
#define __builtin_ceil ceil
//#define __builtin_cexp cexp
//#define __builtin_cexpf cexpf
//#define __builtin_cexpl cexpl
//#define __builtin_cimag cimag
//#define __builtin_cimagf cimagf
//#define __builtin_cimagl cimagl
//#define __builtin_clog clog
//#define __builtin_clogf clogf
//#define __builtin_clogl clogl
//#define __builtin_conj conj
//#define __builtin_conjf conjf
//#define __builtin_conjl conjl
#define __builtin_copysign copysign
#define __builtin_copysignf copysignf
#define __builtin_copysignl copysignl
#define __builtin_cos cos
#define __builtin_cosh cosh
//#define __builtin_cpow cpow
//#define __builtin_cpowf cpowf
//#define __builtin_cpowl cpowl
//#define __builtin_cproj cproj
//#define __builtin_cprojf cprojf
//#define __builtin_cprojl cprojl
//#define __builtin_creal creal
//#define __builtin_crealf crealf
//#define __builtin_creall creall
//#define __builtin_csin csin
//#define __builtin_csinf csinf
//#define __builtin_csinh csinh
//#define __builtin_csinhf csinhf
//#define __builtin_csinhl csinhl
//#define __builtin_csinl csinl
//#define __builtin_csqrt csqrt
//#define __builtin_csqrtf csqrtf
//#define __builtin_csqrtl csqrtl
//#define __builtin_ctan ctan
//#define __builtin_ctanf ctanf
//#define __builtin_ctanh ctanh
//#define __builtin_ctanhf ctanhf
//#define __builtin_ctanhl ctanhl
//#define __builtin_ctanl ctanl
#define __builtin_erf erf
#define __builtin_erfc erfc
#define __builtin_erfcf erfcf
#define __builtin_erfcl erfcl
#define __builtin_erff erff
#define __builtin_erfl erfl
#define __builtin_exit exit
#define __builtin__Exit _Exit
#define __builtin_exp exp
#define __builtin_exp2 exp2
#define __builtin_exp2f exp2f
#define __builtin_exp2l exp2l
#define __builtin_expm1 expm1
#define __builtin_expm1f expm1f
#define __builtin_expm1l expm1l
#define __builtin_fabs fabs
#define __builtin_fdim fdim
#define __builtin_fdimf fdimf
#define __builtin_fdiml fdiml
#define __builtin_floor floor
#define __builtin_fma fma
#define __builtin_fmaf fmaf
#define __builtin_fmal fmal
#define __builtin_fmax fmax
#define __builtin_fmaxf fmaxf
#define __builtin_fmaxl fmaxl
#define __builtin_fmin fmin
#define __builtin_fminf fminf
#define __builtin_fminl fminl
#define __builtin_fmod fmod
#define __builtin_fprintf fprintf
#define __builtin_fputs fputs
#define __builtin_free free
#define __builtin_frexp frexp
#define __builtin_fscanf fscanf
#define __builtin_hypot hypot
#define __builtin_hypotf hypotf
#define __builtin_hypotl hypotl
#define __builtin_ilogb ilogb
#define __builtin_ilogbf ilogbf
#define __builtin_ilogbl ilogbl
#define __builtin_imaxabs imaxabs
#define __builtin_isalnum isalnum
#define __builtin_isalpha isalpha
#define __builtin_isblank isblank
#define __builtin_iscntrl iscntrl
#define __builtin_isdigit isdigit
#define __builtin_isgraph isgraph
#define __builtin_islower islower
#define __builtin_isprint isprint
#define __builtin_ispunct ispunct
#define __builtin_isspace isspace
#define __builtin_isupper isupper
#define __builtin_iswalnum iswalnum
#define __builtin_iswalpha iswalpha
#define __builtin_iswblank iswblank
#define __builtin_iswcntrl iswcntrl
#define __builtin_iswdigit iswdigit
#define __builtin_iswgraph iswgraph
#define __builtin_iswlower iswlower
#define __builtin_iswprint iswprint
#define __builtin_iswpunct iswpunct
#define __builtin_iswspace iswspace
#define __builtin_iswupper iswupper
#define __builtin_iswxdigit iswxdigit
#define __builtin_isxdigit isxdigit
#define __builtin_labs labs
#define __builtin_ldexp ldexp
#define __builtin_lgamma lgamma
#define __builtin_lgammaf lgammaf
#define __builtin_lgammal lgammal
#define __builtin_llabs llabs
#define __builtin_llrint llrint
#define __builtin_llrintf llrintf
#define __builtin_llrintl llrintl
#define __builtin_llround llround
#define __builtin_llroundf llroundf
#define __builtin_llroundl llroundl
#define __builtin_log log
#define __builtin_log10 log10
#define __builtin_log1p log1p
#define __builtin_log1pf log1pf
#define __builtin_log1pl log1pl
#define __builtin_log2 log2
#define __builtin_log2f log2f
#define __builtin_log2l log2l
#define __builtin_logb logb
#define __builtin_logbf logbf
#define __builtin_logbl logbl
#define __builtin_lrint lrint
#define __builtin_lrintf lrintf
#define __builtin_lrintl lrintl
#define __builtin_lround lround
#define __builtin_lroundf lroundf
#define __builtin_lroundl lroundl
#define __builtin_malloc malloc
#define __builtin_memchr memchr
#define __builtin_memcmp memcmp
#define __builtin_memcpy memcpy
#define __builtin_memset memset
#define __builtin_modf modf
#define __builtin_nearbyint nearbyint
#define __builtin_nearbyintf nearbyintf
#define __builtin_nearbyintl nearbyintl
#define __builtin_nextafter nextafter
#define __builtin_nextafterf nextafterf
#define __builtin_nextafterl nextafterl
#define __builtin_nexttoward nexttoward
#define __builtin_nexttowardf nexttowardf
#define __builtin_nexttowardl nexttowardl
#define __builtin_pow pow
#define __builtin_printf printf
#define __builtin_putchar putchar
#define __builtin_puts puts
#define __builtin_realloc realloc
#define __builtin_remainder remainder
#define __builtin_remainderf remainderf
#define __builtin_remainderl remainderl
#define __builtin_remquo remquo
#define __builtin_remquof remquof
#define __builtin_remquol remquol
#define __builtin_rint rint
#define __builtin_rintf rintf
#define __builtin_rintl rintl
#define __builtin_round round
#define __builtin_roundf roundf
#define __builtin_roundl roundl
#define __builtin_scalbln scalbln
#define __builtin_scalblnf scalblnf
#define __builtin_scalblnl scalblnl
#define __builtin_scalbn scalbn
#define __builtin_scalbnf scalbnf
#define __builtin_scalbnl scalbnl
#define __builtin_scanf scanf
#define __builtin_sin sin
#define __builtin_sinh sinh
#define __builtin_snprintf snprintf
#define __builtin_sprintf sprintf
#define __builtin_sqrt sqrt
#define __builtin_sscanf sscanf
#define __builtin_strcat strcat
#define __builtin_strchr strchr
#define __builtin_strcmp strcmp
#define __builtin_strcpy strcpy
#define __builtin_strcspn strcspn
#define __builtin_strlen strlen
#define __builtin_strncat strncat
#define __builtin_strncmp strncmp
#define __builtin_strncpy strncpy
#define __builtin_strpbrk strpbrk
#define __builtin_strrchr strrchr
#define __builtin_strspn strspn
#define __builtin_strstr strstr
#define __builtin_tan tan
#define __builtin_tanh tanh
#define __builtin_tgamma tgamma
#define __builtin_tgammaf tgammaf
#define __builtin_tgammal tgammal
#define __builtin_tolower tolower
#define __builtin_toupper toupper
#define __builtin_towlower towlower
#define __builtin_towupper towupper
#define __builtin_trunc trunc
#define __builtin_truncf truncf
#define __builtin_truncl truncl
#define __builtin_vfprintf vfprintf
#define __builtin_vfscanf vfscanf
#define __builtin_vprintf vprintf
#define __builtin_vscanf vscanf
#define __builtin_vsnprintf vsnprintf
#define __builtin_vsprintf vsprintf
#define __builtin_vsscanf vsscanf

#endif

/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

// Must be the first included header
#ifdef _WIN32
#include "windows.h"
#endif

// Some BSD flavors do not implement all of C99
#if defined(__NetBSD__)
#include <ieeefp.h>
#define FE_DOWNWARD FP_RM
#define FE_UPWARD FP_RP
#define FE_TONEAREST FP_RN
#define FE_TOWARDZERO FP_RZ
#define fegetround() fpgetround()
#define fesetround(RM) fpsetround(RM)
#else
#include <fenv.h>
#endif

#include <ctype.h>
#include <math.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/mlvalues.h"

// Rounding modes supported by Frama-C. Must be synchronized with
// the type [rounding] of the [Floating_point] module.
typedef enum { Nearest_even, Upward, Downward, Toward_zero } rounding_mode;

// Convert Frama-C rounding modes to fenv.h rounding modes.
int fenv_mode(value frama_c_mode) {
  switch ((rounding_mode)Int_val(frama_c_mode)) {
  case Nearest_even:
    return FE_TONEAREST;
  case Upward:
    return FE_UPWARD;
  case Downward:
    return FE_DOWNWARD;
  case Toward_zero:
    return FE_TOWARDZERO;
  default:
    caml_invalid_argument("Invalid rounding mode in [fenv_mode]");
  }
}

// Set the processor rounding mode.
value frama_c_set_round_mode(value frama_c_mode) {
  fesetround(fenv_mode(frama_c_mode));
  return Val_unit;
}

// Return the current rounding mode.
value frama_c_get_round_mode(value _) {
  switch (fegetround()) {
  case FE_TONEAREST:
    return Val_int(Nearest_even);
  case FE_UPWARD:
    return Val_int(Upward);
  case FE_DOWNWARD:
    return Val_int(Downward);
  case FE_TOWARDZERO:
    return Val_int(Toward_zero);
  default:
    caml_invalid_argument("Invalid rounding mode in [frama_c_get_round_mode]");
  }
}

// Floating-point formats supported by Frama-C. Must be synchronized with
// the ['f format] type of the [Floating_point] module, in the sens that it
// must contain at least one value for each constructor of ['f format], and
// those values have to be in the same order. For now, there is also a [Long]
// value, which is not yet used, but will probably be in the future.
typedef enum { Single, Float32, Float64, Double, Long } precision;

// Convert the Frama-C format into a precision.
precision decode_precision(value prec) { return (precision)Int_val(prec); }

// Convert OCaml numerical values into floats, doubles or long doubles.
float to_float(value num) { return Double_val(num); }
double to_double(value num) { return Double_val(num); }
long double to_long(value num) { return Double_val(num); }

// Convert a C double into an OCaml double.
value to_ocaml(double d) { return caml_copy_double(d); }

// Round an ocaml float into single precision.
value round_to_single(value num) { return caml_copy_double(to_float(num)); }

// Round a floating-point number into a given format. It is based on
// C implicit casts to perform the roundings.
value frama_c_change_format(value frama_c_prec, value num) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(to_float(num));
  case Float64:
  case Double:
    return to_ocaml(to_double(num));
  case Long:
    return to_ocaml(to_long(num));
  default:
    caml_invalid_argument(
        "Invalid floating-point format in [frama_c_round_to]");
  }
}

value float_compare_total(value x, value y) {
  union {
    double d;
    int64_t i;
  } ux, uy;
  ux.d = Double_val(x);
  uy.d = Double_val(y);

  if (ux.i == uy.i)
    return Val_int(0);

  ux.i = ux.i ^ (((uint64_t)(ux.i >> 63)) >> 1);
  uy.i = uy.i ^ (((uint64_t)(uy.i >> 63)) >> 1);

  if (ux.i < uy.i)
    return Val_int(-1);
  else
    return Val_int(1);
}

value float_is_negative(value v) {
  union {
    double d;
    uint64_t i;
  } uv;
  uv.d = Double_val(v);
  return (Val_int((int)((uv.i) >> 63)));
}

value frama_c_round(value frama_c_prec, value num) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(roundf(to_float(num)));
  case Float64:
  case Double:
    return to_ocaml(round(to_double(num)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_round]");
    return to_ocaml(roundl(to_long(num)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_round]");
  }
}

value frama_c_trunc(value frama_c_prec, value num) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(truncf(to_float(num)));
  case Float64:
  case Double:
    return to_ocaml(trunc(to_double(num)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_trunc]");
    return to_ocaml(truncl(to_long(num)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_trunc]");
  }
}

value frama_c_exp(value frama_c_prec, value num) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(expf(to_float(num)));
  case Float64:
  case Double:
    return to_ocaml(exp(to_double(num)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_exp]");
    return to_ocaml(expl(to_long(num)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_exp]");
  }
}

value frama_c_log(value frama_c_prec, value num) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(logf(to_float(num)));
  case Float64:
  case Double:
    return to_ocaml(log(to_double(num)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_log]");
    return to_ocaml(logl(to_long(num)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_log]");
  }
}

value frama_c_log10(value frama_c_prec, value num) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(log10f(to_float(num)));
  case Float64:
  case Double:
    return to_ocaml(log10(to_double(num)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_log10]");
    return to_ocaml(log10l(to_long(num)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_log10]");
  }
}

value frama_c_pow(value frama_c_prec, value num, value exp) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(powf(to_float(num), to_float(exp)));
  case Float64:
  case Double:
    return to_ocaml(pow(to_double(num), to_double(exp)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_pow]");
    return to_ocaml(powl(to_long(num), to_long(exp)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_pow]");
  }
}

value frama_c_fmod(value frama_c_prec, value num, value mod) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(fmodf(to_float(num), to_float(mod)));
  case Float64:
  case Double:
    return to_ocaml(fmod(to_double(num), to_double(mod)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_fmod]");
    return to_ocaml(fmodl(to_long(num), to_long(mod)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_fmod]");
  }
}

value frama_c_cos(value frama_c_prec, value num) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(cosf(to_float(num)));
  case Float64:
  case Double:
    return to_ocaml(cos(to_double(num)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_cos]");
    return to_ocaml(cosl(to_long(num)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_cos]");
  }
}

value frama_c_sin(value frama_c_prec, value num) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(sinf(to_float(num)));
  case Float64:
  case Double:
    return to_ocaml(sin(to_double(num)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_sin]");
    return to_ocaml(sinl(to_long(num)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_sin]");
  }
}

value frama_c_tan(value frama_c_prec, value num) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(tanf(to_float(num)));
  case Float64:
  case Double:
    return to_ocaml(tan(to_double(num)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_tan]");
    return to_ocaml(tanl(to_long(num)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_tan]");
  }
}

value frama_c_acos(value frama_c_prec, value num) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(acosf(to_float(num)));
  case Float64:
  case Double:
    return to_ocaml(acos(to_double(num)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_acos]");
    return to_ocaml(acosl(to_long(num)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_acos]");
  }
}

value frama_c_asin(value frama_c_prec, value num) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(asinf(to_float(num)));
  case Float64:
  case Double:
    return to_ocaml(asin(to_double(num)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_asin]");
    return to_ocaml(asinl(to_long(num)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_asin]");
  }
}

value frama_c_atan(value frama_c_prec, value num) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(atanf(to_float(num)));
  case Float64:
  case Double:
    return to_ocaml(atan(to_double(num)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_atan]");
    return to_ocaml(atanl(to_long(num)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_atan]");
  }
}

value frama_c_atan2(value frama_c_prec, value l, value r) {
  switch (decode_precision(frama_c_prec)) {
  case Single:
  case Float32:
    return to_ocaml(atan2f(to_float(l), to_float(r)));
  case Float64:
  case Double:
    return to_ocaml(atan2(to_double(l), to_double(r)));
  case Long:
    caml_failwith("Long double support is not implemented in [frama_c_atan2]");
    return to_ocaml(atan2l(to_long(l), to_long(r)));
  default:
    caml_invalid_argument("Invalid floating-point format in [frama_c_atan2]");
  }
}

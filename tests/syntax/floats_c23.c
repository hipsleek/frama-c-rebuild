/* run.config*
   STDOPT: +"-print"
   STDOPT: +"-machdep gcc_x86_64 -print -cpp-extra-args='-D GCC_EXTENSION'"
*/
#include <math.h>
#include <stddef.h>

int main() {
#ifdef GCC_EXTENSION
  _Float32 f32 = 1.25f32 + 3.F32;
  _Float64 f64 = -1.25f64 - 3.F64;
  _Float64 zero64 = 0.0f64;
  //@ assert \eq_float64(zero64, 0.0f64);
#else
  _Float32 f32 = 1.25f + 3.F;
  _Float64 f64 = -1.25 - 3.;
  _Float64 zero64 = 0.0;
#endif

#ifdef GCC_EXTENSION
  size_t a = _Alignof(f64);
#else
  size_t a = _Alignof(_Float64);
#endif
  //@ assert \eq_float64((_Float64)(f32 + f64), (_Float64) 0.0);
  //@ assert \le_float32((_Float32)f64, f32);

  //@ assert 0.0f32 == 0.0F64;
  _Float32 hex64 = 0x1p2F64;
#ifdef GCC_EXTENSION
  double zerod = 1.5d;
  double hexd = 0x1p2D;
  //@ assert 0.0d == -0.0F64;
#endif

  long double ld = 1.0l;
  return 0;
}

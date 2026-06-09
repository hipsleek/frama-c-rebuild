/* run.config*
   STDOPT: +"-kernel-warn-key=parser:decimal-float=inactive"
   STDOPT: +"-warn-special-float none -kernel-warn-key=parser:decimal-float=inactive"
*/

volatile _Float32 any_f32;
volatile _Float64 any_f64;

struct floats {
  float f;
  _Float32 f32;
  _Float64 f64;
  double d;
};

struct floats arr[10] = {{0.125f, 0.25f32, 0.3f64, 0.4}};

int main() {
  _Float32 m32 = -1.f32;
  _Float32 h32 = .25f32*2.0f32; // 0.5f32
  if (any_f32) {
    _Float32 inf32 = 1.0f32/0.0f32 + 0.125f32 - 1000.f32;
    /*@ check invalid: \is_minus_infinity(inf32); */
    /*@ check valid: \is_plus_infinity(inf32); */
  }
  /*@ check invalid: \eq_float32(m32, h32); */
  /*@ check valid:   \ne_float32(m32, h32); */
  /*@ check valid:   \lt_float32(m32, h32); */
  /*@ check invalid: \ge_float32(m32, h32); */
  /*@ check valid: \is_finite(h32); */
  /*@ check invalid: \is_infinite(m32); */
  /*@ check invalid: \is_NaN(m32); */
  /*@ check valid: \sign(m32) == \Negative; */
  /*@ check not_yet: \exact(m32) == -1.; */
  /*@ check not_yet: \round_error(m32) == 0.; */
  /*@ check not_yet: \total_error(m32) == 0.; */
  /*@ check not_yet: \relative_error(m32) == 0.; */
  /*@ check not_yet: \model(m32) == 0.; */

  if (987654.f32 - h32 < any_f32) {
    m32 = any_f64 - _Alignof(_Float32) + any_f32;
  }

  _Float64 m64 = -1.f64;
  _Float64 h64 = .25f64*2.0f64; // 0.5f64
  if (any_f64) {
    _Float64 inf64 = 1.0f64/0.0f64 + 0.125f64 - 1000.f64;
    /*@ check invalid: \is_minus_infinity(inf64); */
    /*@ check valid: \is_plus_infinity(inf64); */
  }
  /*@ check invalid: \eq_float64(m64, h64); */
  /*@ check valid:   \ne_float64(m64, h64); */
  /*@ check valid:   \lt_float64(m64, h64); */
  /*@ check invalid: \ge_float64(m64, h64); */
  /*@ check valid: \is_finite(h64); */
  /*@ check invalid: \is_infinite(m64); */
  /*@ check invalid: \is_NaN(m64); */
  /*@ check valid: \sign(m64) == \Negative; */
  /*@ check not_yet: \exact(m64) == -1.; */
  /*@ check not_yet: \round_error(m64) == 0.; */
  /*@ check not_yet: \total_error(m64) == 0.; */
  /*@ check not_yet: \relative_error(m64) == 0.; */
  /*@ check not_yet: \model(m64) == 0.; */

  if (987654.f64 - h64 < any_f64) {
    m64 = any_f32 - _Alignof(_Float64) + any_f64;
  }

  // tests related to casts
  float f = (float)any_f32;
  f = (float)any_f64;
  double d = (double)any_f32;
  d = (double)any_f64;
  m64 = (_Float64)f;
  m64 = (_Float64)any_f32;
  m64 = (_Float64)d;
  m32 = (_Float32)f;
  m32 = (_Float32)d;
  m32 = (_Float32)any_f64;

  return 0;
}

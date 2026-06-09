/* Tests that builtins are not used for functions:
   - with incompatible types.
   - without specification.
   - with a specification missing default behavior.
   - with a specification missing assigns clauses. */

/* Integers instead of floating-point numbers. */
int pow (int a, int b) {
  int x = 1;
  //@ loop unroll 10;
  for (int i = 0; i < b; i++)
    x = x * a;
  return x;
}

/* Float instead of double. */
/*@ assigns \result \from f; */
float exp (float f);

/* No assigns clause. */
/*@ requires finite_positive_arg: \is_finite(d) && d >= -0.;
    ensures finite_positive_result: \is_finite(\result) && \result >= -0.; */
extern double sqrt(double d);

/* No specification. */
extern double ceil(double d);

/* No default behavior. */
/*@
  behavior positive:
    assumes d >= 0;
    assigns \result \from d;
  behavior negative:
    assumes d < 0;
    assigns \result \from d; */
extern double floor(double d);

/* All values should be imprecise, as builtins are not used. */
int main(void) {
  int n = pow(2, 4);
  float f = exp(1.5f);
  double two = sqrt(4.);
  double three = floor(3.14);
  double four = ceil(3.14);
}

/*@ predicate is_same_sign_d(double X, double Y) = \sign(X) == \sign(Y);
    predicate is_same_sign_f(float  X, float  Y) = \sign(X) == \sign(Y);
*/

/*@ ensures is_same_sign_d(a_double, \result) ; */
double d (double a_double) {
  return a_double;
}

/*@ ensures is_same_sign_f(a_float , \result) ; */
float  f (float  a_float ) {
  return a_float ;
}

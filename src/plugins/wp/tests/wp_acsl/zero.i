/* run.config
  OPT:
  OPT:  -wp-model real
*/

/* run.config_qualif
  OPT:
  OPT: -wp-model real
*/

//@ predicate Foo(double y) = y == 0.0;

void foo(double x) {
  //@ assert Foo((double) (0.0 + 0.0 * x));
}

/*@
  requires \valid(s+(0..9));
  assigns s[0..9];
*/
void init(double *s) {
  unsigned int i;
  /*@
    loop invariant 0 <= i <= 10;
    loop invariant is_zero: \forall integer k; 0 <= k < i ==> s[k] == 0;
    loop assigns i, s[0..9];
    loop variant 10-i;
  */
  for(i=0; i < 10; i++) {
    s[i] = 0;
  }
}

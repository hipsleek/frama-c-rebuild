/* run.config
   COMMENT: based on WP tutorial by Allan Blanchard:
   COMMENT: Introduction to C program proof with Frama-C and its WP plugin
   STDOPT: +"-eva-unroll-recursive-calls 5"
*/

int zero = 0;

/*@

  inductive even(ℤ x) {
    case zero: \forall ℤ a; even(zero);
    case pos: \forall ℤ a; a >= 0 ==> even(a) ==> even(a+2);
    case neg: \forall ℤ a; a <= 0 ==> even(a) ==> even(a-2);
  }

@*/

int main() {
  /*@ assert even(2); @*/
  /*@ assert !even(3); @*/
  /*@ assert even(-4); @*/
  /*@ assert !even(-3); @*/
  return 0;
}

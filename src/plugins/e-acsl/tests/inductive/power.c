/* run.config
   COMMENT: based on src/plugins/wp/tests/wp_plugin/tutorial.i
   STDOPT: +"-eva-unroll-recursive-calls 5"
*/

/*@

  inductive power(ℤ x, ℤ y, ℤ p) {
      case zero: \forall ℤ a; power(a, 0, 1);
      case non_zero: \forall ℤ a,b,q; power(a,b-1,q) ==> power(a,b,q*a);
  }

  // using a division in the conclusion
  inductive power2(ℤ x, ℤ y, ℤ p) {
      case zero: \forall ℤ a; power2(a, 0, 1);
      case non_zero: \forall ℤ a,b,q; power2(a,b-1,q) ==> power2(a/1,b,q*a);
  }

@*/

int main() {
  /*@ assert power(2,3,8); @*/
  /*@ assert power2(2,3,8); @*/
  /*@ assert !power(2,3,7); @*/
  /*@ assert !power2(2,3,7); @*/
  return 0;
}

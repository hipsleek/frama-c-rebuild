/* run.config
   COMMENT: from Why3 stdlib
   STDOPT: +"-eva-unroll-recursive-calls 5"
*/
enum sign_t { POS, ZERO, NEG };

/*@

    inductive signum(ℝ x, enum sign_t r) {
        case zero:
           signum(0., ZERO);

        case pos:
        \forall ℝ a;
            a > 0. ==> signum(a, POS);

         case neg:
        \forall ℝ a;
            a < 0. ==> signum(a, NEG);
    }

*/

int main() {
  /*@ assert signum(2., POS); */
  /*@ assert signum(0., ZERO); */
  /*@ assert !signum(0.-2., ZERO); */
  return 0;
}

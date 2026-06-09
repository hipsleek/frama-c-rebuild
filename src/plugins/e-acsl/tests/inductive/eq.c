/* run.config
   STDOPT: +"-eva-unroll-recursive-calls 9"
*/

// untranslatable
/*@
    inductive eqA(ℤ x, ℤ y) {
      case eqA13: eqA(3,1);
      case eqA23: eqA(3,2);
      case rightEuclidean: ∀ ℤ a, ℤ b, ℤ c; eqA(c,a) ⇒ eqA(c,b) ⇒ eqA(a,b);
    }
*/

// untranslatable
/*@
    inductive eqB(ℤ x, ℤ y) {
      case eqB13: eqB(1,3);
      case eqB23: eqB(2,3);
      case leftEuclidean: ∀ ℤ a, ℤ b, ℤ c; eqB(a,c) ⇒ eqB(b,c) ⇒ eqB(a,b);
    }
*/

// translatable, but unsound because of overlap
/*@
    inductive eqC(ℤ x, ℤ y) {
      case eq12: eqC(1,2);
      case eq23: eqC(2,3);
      case trans: ∀ ℤ a, ℤ b, ℤ c; eqC(a,b) ⇒ eqC(b,c) ⇒ eqC(a,c);
    }
*/

int main() {
  /*@ assert eqA(1,2); */ // untranslatable
  /*@ assert eqB(1,2); */ // untranslatable
  /*@ assert eqC(1,3); */ // fails due to overlap
  return 0;
}

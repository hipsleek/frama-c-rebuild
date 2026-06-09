/* run.config
   COMMENT: based on example from ACSL specification
   STDOPT: +"-eva-unroll-recursive-calls 5"
*/

/*@

    inductive gcd(ℤ a, ℤ b, ℤ r) {
      case zero: ∀ ℤ x; gcd(x, 0, x);
      case succ:
        ∀ ℤ x, y, z;
          y != 0 ==> gcd(y, x % y, z) ==> gcd(x, y, z);
    }

    inductive gcd2(ℤ a, ℤ b, ℤ r) {
      case eq: ∀ ℤ x; gcd2(x, x, x);
      case gt: ∀ ℤ x, y, r; x > y ==> gcd2(x - y, y, r) ==> gcd2(x, y, r);
      case lt: ∀ ℤ x, y, r; x < y ==> gcd2(x, y - x, r) ==> gcd2(x, y, r);
    }

*/

int main() {
  /*@ assert gcd(42, 24, 6); */
  /*@ assert !gcd(42, 24, 7); */
  /*@ assert gcd2(42, 24, 6); */
  /*@ assert !gcd2(42, 24, 7); */
  return 0;
}

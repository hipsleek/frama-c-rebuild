/* run.config
   COMMENT: The same function is translated twice with different signatures.
   COMMENT: This regression test ensures that translations of sub-terms do not
   COMMENT: spill over from one translation to the other, falling out of scope.
*/

/*@

logic ℤ f(ℤ x) =
  x ≡ 0 ? 0 :
    \let v = f(0);
     0 ≡ v ? x - 1 : 0;

*/

int main() {
  /*@ assert f(1) == 0; */
}

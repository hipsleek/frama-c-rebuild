/* run.config
   COMMENT: fully supported in complete mode
   STDOPT: +"-eva-unroll-recursive-calls 5"
*/

/*@

    inductive add(ℤ n, ℤ m, ℤ r) {
        case add_zero:
        \forall ℤ a;
            add(a, 0, a);

        case add_pos:
        \forall ℤ a, b, c;
            b > 0 ==>
            add(a, b - 1, c - 1) ==>
            add(a, b, c);

        case add_neg:
        \forall ℤ a, b, c;
            b < 0 ==>
            add(a, b + 1, c + 1) ==>
            add(a, b, c);
    }

*/

int main() {
  /*@ assert add(2, 3, 5); */
  /*@ assert !add(2, 3, 4); */
  return 0;
}

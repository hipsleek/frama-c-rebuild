/* run.config
   COMMENT: generated from tests/axiomatic_function/sum.c
   STDOPT: +"-eva-unroll-recursive-calls 9"
*/
/* run.config_dev
   MACRO: ROOT_EACSL_GCC_OPTS_EXT --no-assert-print-data
   COMMENT: ptr values may change for each run
*/

/* logic function that calculates the sum of the elements between indices [low]
   and [high] (inclusively) of an array [a] of length [len] */

/*@

  inductive sum(int *arr, ℤ low, ℤ high, ℤ len, ℤ res) {
    case base: ∀ ℤ low, ℤ high, ℤ len, int *a;
                  low > high ⇒ sum(a, low, high, len, 0);
    case ind: ∀ ℤ low, ℤ high, ℤ len, int *a, ℤ lres;
                    0 ≤ low ≤ high < len ⇒
                    sum(a, low, high - 1, len, lres) ⇒
                    sum(a, low, high, len, a[high] + lres);
  }

*/

int main() {
  int a[4] = {1, 2, 3, 4};
  int *ptr = &a[0];
  /*@ assert sum(ptr, 1, 2, 4, 5); */
  /*@ assert !sum(ptr, 1, 2, 4, 4); */
}

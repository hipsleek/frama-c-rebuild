/* Harder loops — nesting, calls, and nonlinear invariants.
 *
 * Everything here verifies SUCCESS. Each one needed a stronger invariant than
 * the loop_arith.c examples. None of them is vacuous — perturbing a spec makes
 * HipSleek reject it, checked for each: `twice` with `c' = 2*n + 1`, `mult`
 * with `r' = a*b + 1`, and `divide` with the strict `res*d < n` (wrong only
 * when d divides n exactly) all FAIL. See loop_bad.c for the negatives kept
 * as permanent demos.
 */

/* --- Nested loops -------------------------------------------------------
 * Each loop needs its own [SL_loop] spec, and each must be an invariant in
 * its own right. Note the inner spec is written over an unconstrained `j`
 * (`j <= 2`), not `j = 0`: the inner loop's recursive call re-checks it with
 * j already incremented, so `requires j = 0` would fail on the second pass —
 * even though the outer loop always enters it with j = 0.
 */
/*[SL]
  requires n >= 0
  ensures res = 2 * n;
*/
int twice(int n) {
  int i = 0;
  int j = 0;
  int c = 0;
  /*[SL_loop]
     requires c = 2 * i & i <= n
     ensures i' = n & c' = 2 * n;
  */
  while (i < n) {
    j = 0;
    /*[SL_loop]
       requires j <= 2
       ensures j' = 2 & c' = c + 2 - j;
    */
    while (j < 2) {
      c = c + 1;
      j = j + 1;
    }
    i = i + 1;
  }
  return c;
}

/* --- A call inside a loop body ------------------------------------------
 * The callee is verified separately against its own contract; the loop then
 * reasons from that contract, not from inc's body.
 */
/*[SL]
  requires true
  ensures res = x + 1;
*/
int inc(int x) {
  return x + 1;
}

/*[SL]
  requires n >= 0
  ensures res = n;
*/
int call_loop(int n) {
  int i = 0;
  /*[SL_loop]
     requires i <= n
     ensures i' = n;
  */
  while (i < n) {
    i = inc(i);
  }
  return i;
}

/* --- Nonlinear: multiplication by repeated addition ----------------------
 * The invariant `r = a * i` is nonlinear (a and i are both variables), which
 * the Z3 backend discharges. This is the classic case where the invariant
 * carries the whole proof: the exit state i = b turns it into r = a * b.
 */
/*[SL]
  requires b >= 0
  ensures res = a * b;
*/
int mult(int a, int b) {
  int r = 0;
  int i = 0;
  /*[SL_loop]
     requires r = a * i & i <= b
     ensures i' = b & r' = a * b;
  */
  while (i < b) {
    r = r + a;
    i = i + 1;
  }
  return r;
}

/* --- Nonlinear: integer division by repeated subtraction -----------------
 * The loop spec relates entry and exit across an unknown iteration count:
 * `n = q'*d - q*d + n'` says the quotient advanced by exactly the number of
 * subtractions. The function then states the real division contract, that the
 * result q satisfies q*d <= n < (q+1)*d. In the FUNCTION's ensures, unprimed
 * `n` is the pre-state value, which is what makes that claim meaningful even
 * though the loop overwrites n.
 */
/*[SL]
  requires d > 0 & n >= 0
  ensures res * d <= n & n < (res + 1) * d;
*/
int divide(int n, int d) {
  int q = 0;
  /*[SL_loop]
     requires d > 0 & n >= 0
     ensures n' >= 0 & n' < d & n = q' * d - q * d + n';
  */
  while (n >= d) {
    n = n - d;
    q = q + 1;
  }
  return q;
}

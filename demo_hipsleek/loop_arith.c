/* Arithmetic while loops — the basic [SL_loop] spec shapes.
 *
 * A loop spec is NOT a postcondition for one pass: the plugin emits the loop
 * into the .ss, and HipSleek turns it into a recursive procedure whose
 * `requires` is re-checked at the recursive call. So the spec must be a real
 * INVARIANT — it has to hold on entry to every iteration, not just the first.
 * That is why these specs are written over an unconstrained entry state
 * (`requires true`, or an invariant like `s = i`) and case-split on the guard
 * in the `ensures` rather than assuming the caller's initial values.
 * See loop_bad.c (`not_invariant`) for what happens when that is ignored.
 *
 * Primed variables (i') are the post-state; unprimed are the entry state.
 * All functions here verify SUCCESS.
 */

/* Count up to a variable bound n, not just a literal. */
/*[SL]
   requires true
   ensures res >= n;
*/
int count_up(int i, int n) {
  /*[SL_loop]
     requires true
     ensures i < n & i' = n or i >= n & i' = i;
  */
  while (i < n) {
    i = i + 1;
  }
  return i;
}

/* Accumulate: the loop carries the invariant s = i, which is what lets the
   exit state (i = n) pin down the result (s = n). */
/*[SL]
   requires n >= 0
   ensures res = n;
*/
int add_ones(int n) {
  int s = 0;
  int i = 0;
  /*[SL_loop]
     requires s = i & i <= n
     ensures i' = n & s' = n;
  */
  while (i < n) {
    s = s + 1;
    i = i + 1;
  }
  return s;
}

/* Count down: the loop variable is the parameter itself. */
/*[SL]
   requires n >= 0
   ensures res = 0;
*/
int countdown(int n) {
  /*[SL_loop]
     requires n >= 0
     ensures n' = 0;
  */
  while (n > 0) {
    n = n - 1;
  }
  return n;
}

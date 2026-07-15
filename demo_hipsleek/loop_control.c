/* Loop control flow — short-circuit guards, break, and continue.
 *
 * Frama-C's Cil normalisation rewrites all of these before the plugin ever
 * sees them, so each function here is really a test that the plugin puts them
 * back together faithfully:
 *
 *   while (a && b)  ->  Cil: while(1) { if (a) { if (b) {} else break; }
 *                                       else break; ... }
 *   while (a || b)  ->  Cil: while(1) { if (a) {} else { if (b) {} else break; }
 *                                       ... }
 *
 * The plugin walks that nested if/break tree back into a single guard. The
 * guards below are deliberately REDUNDANT (one operand always subsumes the
 * other) so the spec pins down which operand actually binds: if either operand
 * were dropped in translation, the loop would run to the wrong bound and the
 * spec would fail. That makes these genuine fidelity checks, not just parses.
 *
 * All functions here verify SUCCESS.
 */

/* Guard `i < 10 && i < 5` binds at 5, the stronger operand. Drop `&& i < 5`
   and the loop would run to 10, breaking `i' = 5`. */
/*[SL]
   requires i = 0
   ensures res = 5;
*/
int and_guard(int i) {
  /*[SL_loop]
     requires true
     ensures i < 5 & i' = 5 or i >= 5 & i' = i;
  */
  while (i < 10 && i < 5) {
    i = i + 1;
  }
  return i;
}

/* Guard `i < 5 || i < 10` binds at 10, the weaker operand. Drop `|| i < 10`
   and the loop would stop at 5, breaking `i' = 10`. */
/*[SL]
   requires i = 0
   ensures res = 10;
*/
int or_guard(int i) {
  /*[SL_loop]
     requires true
     ensures i < 10 & i' = 10 or i >= 10 & i' = i;
  */
  while (i < 5 || i < 10) {
    i = i + 1;
  }
  return i;
}

/* break: HipSleek models it as an exception caught at the loop boundary. The
   spec must still cover every entry state, hence the three-way case split —
   entering above 5 misses the break and runs to the guard bound instead. */
/*[SL]
   requires i = 0
   ensures res = 5;
*/
int stop_at_five(int i) {
  /*[SL_loop]
     requires true
     ensures i <= 5 & i' = 5 or i > 5 & i < 10 & i' = 10 or i >= 10 & i' = i;
  */
  while (i < 10) {
    if (i == 5) { break; }
    i = i + 1;
  }
  return i;
}

/* continue: caught inside the body, so control still reaches the recursive
   call. The skipped iteration does not change the result. */
/*[SL]
   requires i = 0
   ensures res = 10;
*/
int skip_three(int i) {
  /*[SL_loop]
     requires true
     ensures i < 10 & i' = 10 or i >= 10 & i' = i;
  */
  while (i < 10) {
    if (i == 3) { i = i + 1; continue; }
    i = i + 1;
  }
  return i;
}

/* NEGATIVE cases — every function here has a WRONG spec and must NOT verify.
 * This file is the counterpart to loop_arith.c / loop_control.c: it exists to
 * show that a bogus loop spec is actually rejected rather than waved through.
 *
 * Expected verdicts (`./bin/frama-c -hipsleek demo_hipsleek/loop_bad.c`), as
 * "loop / function" — the plugin reports one line for each:
 *
 *   off_by_one      FAIL    / NOT VERIFIED
 *   wrong_post      SUCCESS / FAIL
 *   not_invariant   FAIL    / NOT VERIFIED
 *   guard_ignored   FAIL    / NOT VERIFIED
 *
 * The two failure modes are different, and the distinction is the point of
 * this file. Verification is modular: HipSleek checks a loop against its own
 * spec, then checks the function while ASSUMING that spec.
 *
 *   - `wrong_post` has a CORRECT loop spec; the function's own claim is false,
 *     so the function itself fails to prove: FAIL.
 *   - The other three have a WRONG loop spec that is nonetheless exactly what
 *     the function needs. Each function's proof is real but rests on a lemma
 *     that was never discharged, so it is reported NOT VERIFIED rather than
 *     SUCCESS, naming the loop it leaned on.
 *
 * NOT VERIFIED is neither "proved" nor "disproved" — the plugin emits it as a
 * don't-know property status, and the Ivette panel shows it amber.
 */

/* WRONG: the loop exits at i = 10, not 9.
   -> loop FAIL. (Function reports SUCCESS: it assumes the bad i' = 9.) */
/*[SL]
   requires i = 0
   ensures res = 9;
*/
int off_by_one(int i) {
  /*[SL_loop]
     requires true
     ensures i < 10 & i' = 9 or i >= 10 & i' = i;
  */
  while (i < 10) {
    i = i + 1;
  }
  return i;
}

/* WRONG: the loop spec is correct, but the function claims 11 and returns 10.
   -> loop SUCCESS, function FAIL. */
/*[SL]
   requires i = 0
   ensures res = 11;
*/
int wrong_post(int i) {
  /*[SL_loop]
     requires true
     ensures i < 10 & i' = 10 or i >= 10 & i' = i;
  */
  while (i < 10) {
    i = i + 1;
  }
  return i;
}

/* WRONG: `requires i = 0` is a precondition, not an invariant. It holds on the
   first iteration only — HipSleek compiles the loop to a recursive procedure
   and re-checks `requires` at the recursive call, where i is already 1. This
   is the most common way to get a loop spec wrong.
   -> loop FAIL. (Function reports SUCCESS: it assumes the bad i' = 10.) */
/*[SL]
   requires i = 0
   ensures res = 10;
*/
int not_invariant(int i) {
  /*[SL_loop]
     requires i = 0
     ensures i' = 10;
  */
  while (i < 10) {
    i = i + 1;
  }
  return i;
}

/* WRONG: the guard is `i < 10 && i < 5`, so the loop stops at 5, not 10. The
   spec is what you would write if the `&& i < 5` operand were ignored, so this
   is the negative twin of loop_control.c's `and_guard`.
   -> loop FAIL. (Function reports SUCCESS: it assumes the bad i' = 10.) */
/*[SL]
   requires i = 0
   ensures res = 10;
*/
int guard_ignored(int i) {
  /*[SL_loop]
     requires true
     ensures i < 10 & i' = 10 or i >= 10 & i' = i;
  */
  while (i < 10 && i < 5) {
    i = i + 1;
  }
  return i;
}

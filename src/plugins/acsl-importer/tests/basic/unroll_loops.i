/* run.config
   STDOPT: -acsl-import %{dep:./unroll_loops.acsl} -ulevel -1 -then -print
   STDOPT: -acsl-import %{dep:./unroll_loops.acsl} -then -print
   STDOPT: -ulevel 2 -typecheck -then -print
   STDOPT: -acsl-import %{dep:./unroll_loops.acsl} -acsl-import-debug 3 -acsl-import-msg-key trace-transformations -ulevel -1 -acsl-import-ulevel-spec for_loops_only:2 -then -print
   STDOPT: -acsl-import %{dep:./unroll_loops.acsl} -ulevel -1 -acsl-import-ulevel-spec for:2 -then -print
   STDOPT: -acsl-import %{dep:./unroll_loops.acsl} -ulevel -1 -acsl-import-ulevel-spec for_loops_only:1,for:2,do-while@main:1,1@while_loops_only:3 -then -print
   STDOPT: -acsl-import %{dep:./unroll_loops.acsl} -ulevel  0 -acsl-import-ulevel-spec for_loops_only:2 -then -print
   STDOPT: -acsl-import %{dep:./unroll_loops.acsl} -ulevel  0 -acsl-import-ulevel-spec for:2 -then -print
   STDOPT: -acsl-import %{dep:./unroll_loops.acsl} -ulevel  0 -acsl-import-ulevel-spec for_loops_only:1,for:2 -then -print
   STDOPT: -acsl-import %{dep:./unroll_loops.acsl} -ulevel -1 -acsl-import-ulevel-spec for_loops_only:1,for:2 -ulevel 0 -then -print
   STDOPT: -acsl-import %{dep:./unroll_loops.acsl} -ulevel -1 -acsl-import-ulevel-spec for_loops_only:1,for:2 -then -ulevel 0 -then -print
   STDOPT: -acsl-import %{dep:./unroll_loops.acsl} -acsl-import-unroll-loop-conditions -then -print
   STDOPT: -acsl-import %{dep:./unroll_loops.acsl} -acsl-import-unroll-loop-conditions -acsl-import-ulevel-spec F_BoucleDepliee_1:1 -then -print
   STDOPT: -acsl-import-unroll-loop-conditions -acsl-import-ulevel-spec F_BoucleDepliee_1:completely@1 -then -print
   STDOPT: -acsl-import-ulevel-spec ":2" -typecheck -then -print
   STDOPT: -acsl-import-ulevel-spec ":2" -typecheck -then -acsl-import %{dep:./unroll_loops.acsl} -then -print
 */


/* Note:
 * -ulevel <n>: performs loop unrolling when <n> is not negative.
 *    that option is processed before importing ACSL specification files.
 *    The transformation is only done just after parsing all C files.
 * Tests:
 * .0.res -> no unrolling.
 * .1.res -> just unrolls loops having a LOOP_UNROLL pragma.
 * .2.res -> unrolls all loops using -ulevel value when there is no LOOP_UNROLL pragma.
 * .14.res -> unrolls all loops using -ulevel-spec ":value" (idem -ulevel value) but missing LOOP_UNROLL pragma.
 * .15.res -> idem previous, but performs importation.
 */

/* Note:
 * -acsl-import-ulevel-spec <unrolling-specification> : adds LOOP_UNROLL pragma.
 *    that option is processed before -ulevel option.
 *    Unrolling loops is left to -ulevel and -acsl-import-ulevel option.
 * .3.res -> insert LOOP_UNROLL pragma for the loops of function 'for_loops_only'.
 * .4.res -> insert LOOP_UNROLL pragma for all loops 'for'.
 * .5.res -> insert LOOP_UNROLL pragma ...
 */

/* Note:
 *   about combination of -acsl-import-ulevel-spec and -ulevel  0 .
 *   So, the unrolling is performed before importing  ACSL specification files.
 * .6.res -> performs unrolling of result.3.res .
 * .7.res -> performs unrolling of result.4.res .
 * .8.res -> performs unrolling of result.5.res
 */

/* Note: -acsl-import-ulevel <n> : performs also loop unrolling when <n> is not negative.
 *    that option is processed after importing ACSL specification files.
 * .9.res -> idem result.8.res except assertion of the loop body is also unrolled.
 * .10.res -> idem result.9.res, but the test show that the unrolling process can be postponed
              living times for another plugins to introduce other specifications.
 */

int x;
void main (void) {
  int i;
  //@ loop variant i - 10 ;
  for (i=0 ; i< 10 ; i++) {
    //@ assert i >= 0 ;
    i++;
  L: ;
  }
  //@ loop unfold 1;
  while (i<20) i++ ;
  do i++ ; while (i < 0) ;
}


void for_loops_only (void) {
  int i;
  for (i=0 ; i< 10 ; i++);
}

void while_loops_only (void) {
  int i = 0;
  while (i< 10)
    i++;
}

enum { NB_ELTS = 2 } ;
int tab5 [NB_ELTS] ;
void F_BoucleDepliee_1(void) {
  int indice;
  for (indice=0; indice<NB_ELTS; indice++) {
    tab5[indice] = 10* indice ;
  }
}

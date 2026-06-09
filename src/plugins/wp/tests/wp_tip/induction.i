/* run.config
   DONTRUN:
*/

/* run.config_qualif
   DEPS: @PTEST_DEPS@ @WP_SESSION@/script/lemma_*.json
   OPT: -wp-prover script,Alt-Ergo -wp-timeout 1 @USING_WP_SESSION@
   DEPS: @PTEST_DEPS@ @WP_SESSION@/script/lemma_*.json
   OPT: -wp-prover script,Alt-Ergo -wp-timeout 1 @USING_WP_SESSION@
   DEPS: @PTEST_DEPS@ @WP_SESSION@/script/lemma_*.json
   OPT: -wp-prover script,Alt-Ergo -wp-timeout 1 @USING_WP_SESSION@
*/

// Script 0: induction on f(x) => success
// Script 1: induction on x => unsuccess
// Script 2: induction on y => unsuccess

/*@
  axiomatic Inductive {

  logic integer f(integer x);
  predicate P(integer x, integer y);

  axiom Hbse: \forall integer y; P(0,y);
  axiom Hsup: \forall integer i,y; 0 <= i ==> P(i,y) ==> P(i+1,y);
  axiom Hinf: \forall integer i,y; i <= 0 ==> P(i,y) ==> P(i-1,y);

  lemma ByInd: \forall integer x,y; P(f(x),y);

  }
*/

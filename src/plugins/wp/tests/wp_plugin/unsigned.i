/* run.config
   DONTRUN:
*/

/* run.config_qualif
   DEPS: @PTEST_DEPS@ @WP_SESSION@/script/lemma_*.json
   OPT: -wp-prover script @USING_WP_SESSION@
*/

/*@
  lemma U32:
  \forall unsigned int x; (x & ((1 << 32)-1)) == x ;
 */

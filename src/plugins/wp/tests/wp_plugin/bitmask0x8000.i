/* run.config
   DONTRUN:
 */

/* run.config_qualif
   DEPS: @PTEST_DEPS@ @WP_SESSION@/script/lemma_*.json
   OPT: -wp-prover script,Alt-Ergo @USING_WP_SESSION@
 */

typedef unsigned short ushort;

/*@
  lemma res_n: \forall ushort off; ! (off & 0x8000) ==> off < 0x8000;
  lemma res_y: \forall ushort off; (off & 0x8000) ==> off >= 0x8000;
*/

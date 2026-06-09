/* run.config_qualif
   DEPS: @PTEST_DEPS@ @WP_SESSION@/script/job_ensures.json
   OPT: -wp-prover script,alt-ergo @USING_WP_SESSION@
 */

/*@ ensures \result == a * b ; */
int job(int a,int b){
  return (a - 1) * b + b ;
}

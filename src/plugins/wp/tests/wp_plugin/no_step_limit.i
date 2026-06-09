/* run.config
  DONTRUN:
*/
/* run.config_qualif
 DEPS: @PTEST_DEPS@ @PTEST_NAME@.conf
  ENV: WHY3CONFIG @PTEST_DIR@/@PTEST_NAME@.conf
  OPT: -wp-prover no-steps -wp-steps 10 -wp-timeout 1 -wp-cache none
*/

// cache is locally deactivated to be sure that we call the prover

/*@
  lemma truc: \false ;
*/

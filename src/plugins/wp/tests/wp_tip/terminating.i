/* run.config
   DONTRUN:
*/

/* run.config_qualif
   OPT: -wp-status -wp-prover tip -wp-script dry
 */

/*@ lemma dummy: \forall integer i,j; i < 0 ==> j < 0; */

/*@
  strategy FastAltErgo:  \prover("Alt-Ergo", 1); // run Alt-Ergo for 1 sec.
  strategy EagerAltErgo: \prover("Alt-Ergo",10); // run Alt-Ergo for 10 sec.

  strategy FilterProver:
    FastAltErgo,
    \tactic("Wp.filter"
      ,\goal(_)
      ,\children(EagerAltErgo)
    ),
    EagerAltErgo;

  proof FilterProver: dummy;
*/

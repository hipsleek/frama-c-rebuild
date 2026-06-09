/* run.config
   DONTRUN:
*/

/* run.config_qualif
   OPT: -print
 */

// Provers

/*@
  \wp::strategy P1: \prover("Alt-Ergo");
  \wp::strategy P2: \prover(0.5);
  \wp::strategy P3: \prover("Alt-Ergo",3.0);
  \wp::strategy P4: P1, P2, P3, \default;
 */

/* run.config_ce
   OPT: -wp -wp-counter-examples -wp-prover cvc4 -wp-status
*/
/* run.config_qualif
   DONTRUN:
*/


//@ check lemma wrong: \forall integer x; \abs(x) == x;

//@ ensures \result == \abs(x); assigns \nothing;
int wrong(int x) { return x; }

/* run.config
   DONTRUN:
*/

/* run.config_qualif
   OPT: -wp-status -wp-prover tip -wp-script dry
 */

/*@
  axiomatic Observers {
    predicate P(integer x);
  }
*/

/*@
  requires (x < 1000 && y < 1000 || z < 1000 && t < 1000);
  ensures post: P(\result);
  assigns \nothing;
*/
int target(unsigned x, unsigned y, unsigned z, unsigned t)
{
  return x+y+z+t;
}
/*@
  strategy Split: \tactic("Wp.split", \pattern( (A && B) || (C && D) ));
  proof Split: post;
*/

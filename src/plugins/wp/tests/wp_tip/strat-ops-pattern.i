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
  requires ( x == 0 || z != 2 || z <= t || t < y || (z % t == y) );
  ensures post: P(\result);
  assigns \nothing;
*/
int target(unsigned x, unsigned y, unsigned z, unsigned t)
{
  return x+y+z+t;
}
/*@
  strategy Split:
    \tactic("Wp.split"
      ,\pattern( x == 0 || z != 2 || z <= t || t < y || (z % t == y) )
    );
  proof Split: post;
*/

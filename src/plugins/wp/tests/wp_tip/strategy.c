/* run.config
   DONTRUN:
*/

/* run.config_qualif
   OPT: -wp-status -wp-prover tip -wp-script dry -wp-strategy Prover
   OPT: -wp-status -wp-prover tip -wp-script dry -wp-strategy Eager
   OPT: -wp-status -wp-prover tip -wp-script dry -wp-strategy EagerRange
   OPT: -wp-status -wp-prover tip -wp-script dry -wp-strategy Lazy
 */

/*@
  axiomatic Observers {
  predicate P(integer x);
  }
*/

/*@
  \wp::strategy Prover: \prover("Alt-Ergo",0.1);

  \wp::strategy Lazy:
    Prover,
    \tactic("Wp.overflow"
      ,\pattern(\any(to_uint32(_),to_sint32(_)))
    );

  \wp::strategy Eager:
    \tactic("Wp.overflow"
      ,\pattern(\any(to_uint32(_),to_sint32(_)))
    ),
    Prover;

  \wp::strategy EagerRange:
    \tactic("Wp.overflow"
      ,\pattern(\any(to_uint32(_),to_sint32(_)))
      ,\child("In-Range",EagerRange)
      ,\children(Prover)
    ),
    Prover;

*/


/*@
  requires x < 1000 && y < 1000 && z < 1000 && t < 1000;
  ensures P(\result);
  assigns \nothing;
*/
int target(unsigned x, unsigned y, unsigned z, unsigned t)
{
  return x+y+z+t;
}

/* run.config
   DONTRUN:
*/

/* run.config_qualif
   EXIT: 1
   OPT: -wp-status -wp-prover tip -wp-script dry
 */

/*@
  strategy Split4:
    \tactic("Wp.split"
      ,\pattern( _ || _ || _ || _ )
    );
  proof Split4: post;
*/

/*@
  requires x || y || z;
  ensures post: \false;
  assigns \nothing;
*/
void target3_no_match(int x, int y, int z) { return; }

/*@
  requires x || y || z || r;
  ensures post: \false;
  assigns \nothing;
*/
void target4_exact_match(int x, int y, int z, int r) { return; }

/*@
  requires x || y || z || r || s;
  ensures post: \false;
  assigns \nothing;
*/
void target5_extra_match(int x, int y, int z, int r, int s) { return; }

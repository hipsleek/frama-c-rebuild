/* run.config
   DONTRUN:
*/

/* run.config_qualif
   OPT: -wp-status -wp-prover tip -wp-script dry
*/

//@ predicate p1234(\list<integer> l) = l == [| 1 , 2 , 3 , 4 |] ;

/*@
  ensures post1: p1234([| a |] ^ [| b |] ^ [| c |] ^ [| d |]);
  ensures post2: p1234([| a |] *^ 4);
  assigns \nothing;
*/
void target(int a, int b, int c, int d)
{

}


/*@
  strategy Unfold1:
    \tactic("Wp.unfold" ,\pattern( P_p1234(\concat(a, b, c, d)) ));
  proof Unfold1: post1;

  strategy Unfold2:
    \tactic("Wp.unfold" ,\pattern( P_p1234(\repeat(a, 4)) ));
  proof Unfold2: post2;
*/

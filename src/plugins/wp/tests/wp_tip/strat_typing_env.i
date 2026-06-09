/* run.config_qualif
   DONTRUN:
*/

/*@ strategy S:
    \tactic("Wp.modmask", \pattern(x % y));
*/

/*@ strategy X:
    \tactic("Wp.cut",
            \pattern(x != y),
	          \select(x),
	          \param("case", "CASES"));
*/

void foo(int a, int b){
  if(a != b){
    //@ assert P: a % b == 0 ;
  }
}

//@ proof X: P;

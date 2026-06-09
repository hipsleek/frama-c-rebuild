/* run.config
   DONTRUN:
*/

/*@ inductive P(int* p){
      // after Qed simplification :
      // false ==> P(p)
      // true --> ill-formed
      case c: \forall int* p ; \valid(p) && p == (void*)0 ==> P(p);
    }
*/

//@ requires P(p);
void foo(int *p){
  //@ assert \false;
}

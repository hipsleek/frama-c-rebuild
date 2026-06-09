//@ import why3: option::Option \as O ;

//@ logic O::option<integer> to_opt_t(integer p) = O::Some(p);
//@ lemma bar_1: \forall integer p ; to_opt_t(p) == O::Some(p);
//@ lemma bar_2: \forall integer p ; to_opt_t(p) != O::None;

/*@ logic O::option<integer> to_opt(int* p) =
      p == \null ? O::None : O::Some(*p);
*/

int g ;

/*@ behavior zero:
      assumes i == 0 ;
      ensures to_opt(\result) == O::None ;

    behavior other:
      assumes i != 0 ;
      ensures to_opt(\result) == O::Some(42) ;
*/
int* f(int i){
  if(i == 0) return (void*)0;
  else {
    g = 42 ;
    return &g ;
  }
}

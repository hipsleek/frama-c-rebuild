/*@ inductive P<A>(A i, \list<A> s){
    case one<A>: 
      \forall A a, \list<A> s ; \Cons(a, s) == [| a |] ==> P(a, s);
    }
*/

/*@ logic \list<A> foo<A> (A i, \list<A> s) = \Cons (i, s);

    logic \list<integer> bar (integer i, \list<integer> s) =
      foo(i, s);

    check lemma test_ok: \forall integer i; \forall \list<integer> s;
      bar (i, s) == \Cons (i, s);

    check lemma test_ko: \forall integer i; \forall \list<integer> s;
      bar (i, s) != \Cons (i, s);

    check lemma testp_ok <A>: \forall A a; \forall \list<A> s;
      foo (a, s) == \Cons (a, s);

    check lemma testp_ko <A>: \forall A a; \forall \list<A> s;
      foo (a, s) != \Cons (a, s);
*/

/*@ axiomatic X {
      logic \list<A> x_foo<A>(A i, \list<A> s) reads \nothing ;
      logic \list<integer> x_bar (integer i, \list<integer> s) reads \nothing ;

      axiom x_foo_value<A>:
        \forall A i, \list<A> s ;
          x_foo(i, s) == \Cons(i, s) ;

      axiom x_bar_value:
        \forall integer i, \list<integer> s ;
          x_bar(i, s) == x_foo(i, s) ;
    }

   check lemma test2_ok: \forall integer i; \forall \list<integer> s;
      x_bar (i, s) == \Cons (i, s);

    check lemma test2_ko: \forall integer i; \forall \list<integer> s;
      x_bar (i, s) != \Cons (i, s);

    check lemma testp2_ok <A>: \forall A a; \forall \list<A> s;
      x_foo (a, s) == \Cons (a, s);

    check lemma testp2_ko <A>: \forall A a; \forall \list<A> s;
      x_foo (a, s) != \Cons (a, s);
*/

/*@ inductive P<A>(A i, \list<A> s){
    case one<B>:
      \forall B b, \list<B> s ; \Cons(b, s) == [| b |] ==> P(b, s);
    }
*/

/*@ axiomatic Y {
      predicate Y<A>(A i, \list<A> s);
    }
*/

int main(void){
  //@ check NotP_ok: !P(1, [| 1 |]) ;
  //@ check Y_ko: Y(1, [| 1 |]) ;
}

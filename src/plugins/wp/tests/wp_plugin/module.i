/*@
  module foo::Bar {
    type t;
    logic t e;
    logic t op(t x, t y);
    logic t opN(t x, integer n) = n >= 0 ? op(x, opN(x,n-1)) : e;
  }
  module foo::Jazz {
    import foo::Bar \as X;
    logic X::t inv(X::t x);
    logic X::t opN(X::t x, integer n) = n >= 0 ? X::opN(x,n) : X::opN(inv(x),-n);
  }
  import foo::Bar \as A;
  import foo::Jazz \as B;
  lemma AbsOp: \forall foo::Bar::t x, integer n;
    B::opN(x,\abs(n)) == A::opN(x,\abs(n));
 */

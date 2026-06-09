/* run.config
   STDOPT:
   STDOPT: +"-cpp-extra-args='-DILL_TYPED'"
 */

/*@
  module Foo {
    type t;
    logic t e;
    logic t op(t x, t y);
    logic t opN(t x, integer n) = n >= 0 ? op(x, opN(x,n-1)) : e;
  }
  module foo::bar {
    import Foo \as X;
    type a;
    logic a f(a x);
    logic X::t inv(X::t x);
    logic X::t opN(X::t x, integer n) = n >= 0 ? X::opN(x,n) : opN(inv(x),-n);
  }
  module foo::jazz {
    import Foo; // both t and Foo::t can be used
    type a;
    logic a f(a x);
    logic t inv(Foo::t x);
    logic t opN(t x, integer n) = n >= 0 ? opN(x,n) : Foo::opN(inv(x),-n);
  }
  module priority {
    import Foo ;
    import foo::bar ;
    import foo::jazz ;
    logic t inv_jazz(t x) = inv(x); // OK, shall call foo::jazz::opN
    logic a f_jazz(a x) = f(x);     // OK, shall call foo::jazz::f
    logic bar::a f_bar(bar::a y) = bar::f(y); // OK
  }
  module priority_aliased {
    import Foo ;
    import foo::bar \as B;
    import foo::jazz \as J;
    logic t inv_jazz(t x) = J::inv(x);   // OK
    logic J::a f_jazz(J::a x) = J::f(x); // OK
    logic B::a f_bar (B::a y) = B::f(y); // OK
  }
  import Foo \as A;
  import foo::bar \as B;
  lemma AbsOp: \forall Foo::t x, integer n;
    B::opN(x,\abs(n)) == A::opN(x,\abs(n));
 */

#ifdef ILL_TYPED

/*@
  import Foo \as F;
  logic t x = F::e; // ill-formed: t should be F::t
*/

/*@
  import Foo \as F;
  import foo \as f;
  logic F::t x = f::bar::inv(F::e); // OK
  logic F::t y = bar::inv(F::e); // KO
*/

/*@
  module A {
     logic integer a = 0;
     module B {
       logic integer b = a + 1;
     }
  }

import A::B \as b;

logic integer z = b::a; // KO

*/

/*@
  module wrong_priority {
    import Foo ;
    import foo::bar ;
    import foo::jazz ;
    logic a f_ko(a x) = bar::f(x);  // KO, ill typed
  }
*/

/*@
  // Duplicated module
  module Foo {
    type t;
  }
*/

/*@
  // Nested module in axiomatix is not supported yet
  axiomatic p {
    module Bar {
      type t;
    }
  }
*/
#endif

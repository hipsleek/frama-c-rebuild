/*@
  module Foo {
    type t;
    logic t elt;
    module Bar {
      predicate test1(t x) = x == Foo::elt;
      predicate test2(t x, t y) = test1(x) && test1(y);

      module Jazz {
        predicate test3(t x) = test2(x,x);
        predicate test4(t y) = Bar::test1(y);
      }

    }
    predicate test5(t y) = Bar::test2(y,y);
    predicate test6(t y) = Bar::Jazz::test3(y);
    predicate test7(t z) = Foo::Bar::test1(z);
  }
*/

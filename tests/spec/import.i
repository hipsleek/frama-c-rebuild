/* run.config
  MODULE: Test_import
  STDOPT: +"-kernel-msg-key acsl-extension"
  STDOPT: +"-kernel-msg-key acsl-extension -kernel-msg-key printer:imported-modules"
*/

//@ import \myplugin1::foo: A::B;
//@ predicate check1(B::t x) = B::check(x,0);
//@ predicate check2(A::B::t x) = A::B::check(x,0);

//@ import \myplugin1::foo: A::B \as C;
//@ predicate check3(C::t x) = C::check(x,0);
//@ predicate check4(C::t x) = A::B::check(x,0);

//@ import \myplugin2::foo: X;
//@ predicate check5(X::t x) = X::check(x,0);

//@ import bar: X::Y;
//@ predicate check6(Y::t x) = Y::check(x,0);
//@ predicate check7(X::Y::t x) = X::Y::check(x,0);

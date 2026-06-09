/* run.config
  STDOPT:
  EXIT: 1
  STDOPT: #"-cpp-extra-args=-DTOO_MANY_ATTRIBUTES"
*/

void f() {

  foo: __attribute__((unused));

  __attribute__((fallthrough));

  #ifdef TOO_MANY_ATTRIBUTES
  __attribute__((assume(42))) __attribute__((fallthrough));
  #endif
}

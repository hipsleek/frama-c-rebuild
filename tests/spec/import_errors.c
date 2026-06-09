/* run.config
  MODULE: Test_import
  COMMENT: Debug is set to distinguish this particular test in the module
  STDOPT: +"-debug 1 -kernel-msg-key acsl-extension -cpp-extra-args=-DWARNINGS"
  EXIT: 1
  STDOPT: +"-cpp-extra-args=-DERR_DRIVER_AMBIGUITY"
  STDOPT: +"-cpp-extra-args=-DERR_UNKNOWN_DRIVER1"
  STDOPT: +"-cpp-extra-args=-DERR_UNKNOWN_DRIVER2"
*/

#ifdef WARNINGS
// Wrong token identifier (here parsed as BEHAVIORS)
//@ import \myplugin1::behaviors: A;

//@ import \myplugin1::foo: B;

// Silently ignored, duplicated import
//@ import \myplugin1::foo: B;

// Already imported with different driver
//@ import \myplugin1::bar: B;
// Already imported with different plugin
//@ import \myplugin2::foo: B;

/*@
  module C {
    type t;
    predicate check(t x, ℤ k) ;
  }
  // Module C already defined
  import \myplugin1::foo: C;
*/
#endif

#ifdef ERR_DRIVER_AMBIGUITY
//@ import foo: A::B \as D;
#endif

#ifdef ERR_UNKNOWN_DRIVER1
//@ import \myplugin1::toto: E;
#endif

#ifdef ERR_UNKNOWN_DRIVER2
//@ import toto: F;
#endif


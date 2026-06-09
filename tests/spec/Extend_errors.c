/* run.config
  MODULE: @PTEST_NAME@
  COMMENT: Debug is set to distinguish this particular test in the module
  STDOPT: +"-debug 1 -kernel-msg-key acsl-extension"
  STDOPT: +"-cpp-extra-args=-DWARN_EXT_KERNEL"
  STDOPT: +"-cpp-extra-args=-DWARN_EXT_TOKEN"
  EXIT: 1
  STDOPT: +"-cpp-extra-args=-DWARN_EXT_UNKNOWN"
  STDOPT: +"-cpp-extra-args=-DERR_EXT_AMBIGUITY"
*/

/*@ \myplugin1::foo x == 0;
    \myplugin2::foo x == 0;
*/
int ok(int x) {
  return x;
}

// Using a token as extension name
#ifdef WARN_EXT_TOKEN
//@ \myplugin1::__FC_FILENAME__ x == 0;
#endif

#ifdef ERR_EXT_KERNEL
int kernel(int x) {
  //@ \kernel::calls ok;
  return ok(x);
}
#endif

#ifdef WARN_EXT_UNKNOWN
/*@ \myplugin1::bar x == 0;
    \unknown_plugin::bar x == 0;
 */
#endif

// Using an extension registered 2 times or more with different plugins
#ifdef ERR_EXT_AMBIGUITY
//@ foo x == 0;
#endif

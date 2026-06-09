/* run.config
  MODULE: @PTEST_NAME@
  OPT: -kernel-warn-key=annot-error=warning -print
*/

/*@ \test::bhv_foo must_replace(x); */
int f(int x);

/*@ behavior test:
  bhv_foo must_replace(x);
*/
int g(int x);


int f(int x) {
  int s = 0;
  /*@ loop \test::nl_foo must_replace(x); */
  for (int i = 0; i < x; i++) s+=g(i);
  /*@ \test::ca_foo must_replace(x); */
  return s;
}

int k(int z) {
  int x = z;
  int y = 0;
  /*@ \test::ns_foo must_replace(x); */
  y = x++;
  return y;
}

/*@ \test::gl_foo must_replace(x); */

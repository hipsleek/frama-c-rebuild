/* run.config
MODULE: @PTEST_NAME@
STDOPT: -acsl-import %{dep:./@PTEST_NAME@.acsl} -copy -kernel-warn-key annot-error=active -then -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -acsl-import-no-run -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
*/

/*@ \test::foo x == 0;
    \test::bar \result == 0;
    \test::bla \trace(x<10) || \trace(x>40);
 */
int f(int x);

/*@ behavior test:
  \test::foo y == 1;
  \test::bar y + \result == 0;
  \test::bla \trace(y<42) && \trace(y>12);
*/
int g(int y);


int f(int x) {
  int s = 0;
  /*@ loop \test::lfoo i<=x;
      loop \test::baz \at(i,LoopEntry), 0;
   */
  for (int i = 0; i < x; i++) s+=g(i);
  /*@ \test::ca_foo s == 0; */
  return s;
}

/*@ behavior ko:
  \test::baz \true;
*/
int h(int z);

int k(int z) {
  int x = z;
  int y = 0;
  /*@ \test::ns_foo \at(x, Post) == z + 1; */
  L: y = x++;
  return y;
}

/*@ \test::global_foo \forall integer x; x < x + 1
; */

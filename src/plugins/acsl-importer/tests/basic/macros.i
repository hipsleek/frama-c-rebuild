/* run.config
STDOPT: -acsl-import %{dep:./macros.fc} -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */

//@ requires file: \true;
extern void f(int w, int x, int y, int z);

//@ requires file: \true;
void g(int a, int b, int x, int z) {
  f(a, b, x, z) ;
}

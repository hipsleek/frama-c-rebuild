/* run.config
STDOPT: -acsl-import-ulevel-spec for:1 -acsl-import %{dep:./loop-pragma.acsl} -then -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */
int z;
void main (int a) {
  int x, y;
  for (x=0;x<a;x++)
    for (y=0; y<x;y++)
      z++;

}

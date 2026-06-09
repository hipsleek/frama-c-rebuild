/* run.config
STDOPT: -acsl-import %{dep:./merge-spec-1.acsl} -acsl-import %{dep:./merge-spec-2.acsl} -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
*/

int A, B, x1, x2;

void f() {
  if (A) {
    B++;
    x1 = 1;
  }
  else {
    //@ requires just_before_LL: \true;
  LL: 
    //@ requires just_after_LL: \true;
    A++;
    x2 = 2;
  }
}

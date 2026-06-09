/* run.config
PLUGIN: @PTEST_PLUGIN@ eva,scope
STDOPT: -acsl-import %{dep:./demo.acsl} -then -eva
STDOPT: -acsl-import %{dep:./demo-error.acsl} -then -eva
STDOPT: -kernel-warn-key annot-error=active -acsl-import %{dep:./demo-error.acsl} -then -eva
STDOPT: -acsl-import %{dep:./demo.acsl} -then -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */
int f(int i);
int g(int j);

void job(int *t, int A) {

  for(int i = 0; i < 50; i++) t[i] = f(i);

  for(int j = A; j < 100; j++) t[j] = g(j);
}

int T[100];

void main(void) {
  job(T, 50);
  //  job(T, 48);
}

/* run.config
STDOPT: %{dep:./unit1.i} -acsl-import %{dep:./unit1.acsl},%{dep:./unit2.acsl} -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */
static int S2=0;
extern int E;
extern void h(void);
int E2;
void g() {
  if (E>=0) {
    static int F2=0;
    F2++;
    S2++;
    E2++;
    h();
  }
}

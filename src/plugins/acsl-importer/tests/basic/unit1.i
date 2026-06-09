/* run.config
STDOPT: -acsl-import %{dep:./@PTEST_NAME@.acsl} -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */
extern void g(void) ;

void f(int a) {
  if (a) {
    int t = a;
    static int F=0;
    F++;
    g();
  }
}
int E = 0;
static int S = 0;
void h() {
  S++;
  E++;
}

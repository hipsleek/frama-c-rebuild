/* run.config*
  LOG: @PTEST_NAME@_ocode.i
  STDOPT: -print -ocode @PTEST_NAME@_ocode.i
ENABLED_IF: %{bin-available:gcc}
  EXECNOW: LOG @PTEST_NAME@.out LOG @PTEST_NAME@.err gcc %{dep:@PTEST_NAME@_ocode.i} -c -o @DEV_NULL@ > @PTEST_NAME@.out 2> @PTEST_NAME@.err
*/

struct t {
  int a;
};
typedef struct t t;
__attribute__((visibility("hidden"))) t f() {
  t res = {0};
  return res;
}

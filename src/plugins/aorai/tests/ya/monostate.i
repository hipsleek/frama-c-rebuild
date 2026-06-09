/* run.config
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya}
*/
/* run.config_prove
   DONTRUN:
*/
void f(void) {}

void main(void) {
  while (1) f();
}

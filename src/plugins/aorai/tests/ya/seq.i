/* run.config*
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya} -aorai-acceptance
 */

void f() { }

void g() { }

int main(int c) {
  if (c) f();
  g();
  if (c) g();
  return 0;
}

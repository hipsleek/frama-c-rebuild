/* run.config*
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya} -aorai-acceptance
*/

int x=0;

void f (void) { x=3; }

void g (void) { x=4; }

int main () {
  f();
  g();
  f();
  g();
  return x;
}

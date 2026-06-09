/* run.config*
  STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya}
*/

int f(int x) { return x; }

int g(int y) { return y; }

int main() { f(1); g(2); }

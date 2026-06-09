/* run.config*
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya}
*/

void (*pf)(void);

int X;

void f(void) { X++; }

void g(void) { X--; }

int main(int c) {
  if(c) pf = f; else pf = g;
  /*@ calls f,g; */
  pf();
  if (c) pf = g; else pf = f;
  /*@ calls f,g; */
  pf();
}

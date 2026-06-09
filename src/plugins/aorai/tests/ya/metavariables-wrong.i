/* run.config*
 EXIT: 1
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya}
*/
void f(int x) {}
void g(void) {}
void h(void) {}

void main(void)
{
  int x = 0;
  while (x < 100)
  {
    if (x % 2)
      f(x);
    else
      g();
    h();
    x++;
  }
}


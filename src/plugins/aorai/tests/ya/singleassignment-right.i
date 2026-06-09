/* run.config*
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya}
*/

void main(int *x, int *y)
{
  int t = *x;
  *x = *y;
  *y = t;
}

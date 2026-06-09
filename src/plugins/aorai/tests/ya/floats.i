/* run.config*
  STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya} -then-last -eva -eva-warn-key assigns:missing=warning
*/

//@ assigns \result \from x, n;
float square_root_aux(float x, float n) {
    float y = (x + n / x) / 2.0f;
    if (x - y > 0.001)
      return y;
    else
      return square_root_aux(y, n);
}

float square_root(float n) {
  return square_root_aux(1.0, n);
}

void main(void)
{
  float r = square_root(2.0f);
}

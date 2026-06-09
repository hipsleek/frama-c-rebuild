/* Minimal example to test functions [get_results] and [set_results]
   from Eva_results module. Analysis called from test_get_set_results.ml. */

volatile unsigned int nondet;

int g[40];

void test (int a) {
  int x = a+1;
  int y = 2*x + 1;
  //@ check y < 40;
  g[y] = 0; // alarm only from [imprecise] call.
}

void precise (void) {
  test(3);
  test(7);
  test(11);
}

void imprecise (void) {
  test(nondet % 20);
}

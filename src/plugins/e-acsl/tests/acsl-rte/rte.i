/* run.config
 * COMMENT: Check that the RTE guards are generated at the right place.
*/

/*@ ensures
    \let delta = 1;
    \let avg_real = (a+b)/2;
    avg_real - delta < \result < avg_real + delta; */
double avg(double a, double b) {
  return (a + b) / 2;
}

/*@ logic double f2(double x) = (double)(1/x); */

/*@
  requires 1 % a == 1;
  ensures 1 % b == 1;

  behavior bhvr:
    assumes 1 % c == 1;
    requires 1 % d == 1;
    requires (1 % f == 1) || (1 % g == 1);
    requires (1 % h == 1) && (1 % i == 1);
    requires \let var = 1; var % j == 1;
    requires \forall integer var; 0 <= var < k ==> var % k == var;
    requires \exists integer var; 0 <= var < l && var % l == var;
    ensures 1 % e == 1;
*/
void test(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j,
          int k, int l) {}

int main(void) {
  int y = 2;
  long z = 2L;
  int w = 12;

  /*@ assert 4 / 2 == 2; */  // trivial case for division by zero
  /*@ assert (1 == 1) || (1 / y) < 2 ; */ // pathologic case for item #287
  /*@ assert 4 / y == 2 || 1 / w > 0; */
  /*@ assert 4 / (12 + 3 - 6) < 2; */

  /*@ assert 1 + ((z+1) / (y-123456789123456789)) == 1; */

  /*@ assert \forall integer i,j; 0 <= i < 5 / y && 4 <= j < 100 % y ==> j + i < 10; */

  double d = 2.0;
  /*@ assert f2(d) > 0; */

  test(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13);

  char *x;
  unsigned int v = 1;
  /*@ assert \aligned(x,v); */
  /*@ assert \aligned(x,alignof(char)); */

  /*@ assert 12 / (v / (y / z)) > 0; */

  int c = 98;
  int t = 1;
  int f = 2;
  int ff = 1;
  /*@ assert 12 / c ? (1 / t) < 3 : (1 / ((f / ff) - 1)) > 0; */

  int *n = &y;
  /*@ assert 12 / *n > 0; */

  return 0;
}

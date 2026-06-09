/* run.config*
   STDOPT: #""
*/

volatile int nondet;
int X,Y,Z1,Z2,T,U1,U2,V,W1,W2;
int a,b,d1,d2,d0,e;
int t[5]={1,2,3};
int *p;

void test (void) {
  int i;
  volatile int c=0;
  while (c+1)
    {
      if (c) X++;
      if (c+2) X--;
    }
  Y = -5;
  if ((X>=Y)  && (X<=12) )
    Y = X;
  Y = 27 * Y + 9;
  Z1 = Y / 3;
  Z2 = Y / 5;
  V = Y + 1;
  W1 = V / 3;
  W2 = V / 5;
  T = Y + 160;
  U1 = T / 3;
  U2 = T / 5;
  p = &(t[3]);
  a = 40000/Z2;
  b = ((int)&Z2)/Z2;
  d2 = 100 / (int)(&X + 2);
  d1 = 100 / (int)(&X + 1);
  d0 = 100 / (int)(&X);
  e = - (int) &X;
}

/* Tests the emission of overflow alarms on a division x/y only if [x] may be
   equal to MIN_INT and [y] may be equal to -1. Also tests the reduction of the
   possible values of [x] or [y] when possible. Similar test in file modulo.i. */
void test_overflow_alarms (void) {
  int min_int = -2147483648;
  int min_one = -1;
  int a = nondet ? min_int : 10;
  int b = nondet ? -1 : 2;
  int x = nondet;
  int y = nondet;
  int r = 0;
  if (nondet) {
    r = a / b; // Overflow alarm.
    // No reduction as [a] and [b] cannot be reduced simultaneously.
    Frama_C_show_each(a, b);
  }
  if (nondet) {
    r = a / min_one; // Overflow alarm.
    r = a / min_one; // No alarm if [a] has been reduced.
    Frama_C_show_each_ten(a); // Check the reduction of [a].
  }
  if (nondet) {
    r = min_int / b; // Overflow alarm.
    r = min_int / b; // No alarm if [b] has been reduced.
    Frama_C_show_each_two(b); // Check the reduction of [b]
  }
  if (nondet) {
    r = x / min_one; // Overflow alarm.
    r = x / min_one; // No alarm if [x] has been reduced.
    Frama_C_show_each(x); // All integers except MIN_INT.
  }
  if (nondet) {
    r = min_int / x; // Overflow alarm and division by zero alarm.
    // All integers except -1 and 0, but not representable as an interval.
    Frama_C_show_each(x);
  }
  if (nondet) {
    r = min_int / min_one; // Invalid alarm.
    Frama_C_show_each_BOTTOM(min_int, min_one);
  }
  // Overflow alarm and division by zero alarm,
  // no possible reduction: [x] and [y] must be top_int as the end.
  r = x / y;
}

void main (void) {
  test();
  test_overflow_alarms();
}

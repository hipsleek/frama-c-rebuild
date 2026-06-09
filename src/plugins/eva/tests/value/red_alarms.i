/* run.config*
   LOG: @PTEST_NAME@.csv
   STDOPT: +"-eva-report-red-statuses ./@PTEST_NAME@.csv"
*/

volatile int nondet;

void maybe_swap (int *p, int *q) {
  //@ check p != q;
  //@ check *p != *q;
  if (nondet) {
    int v = *p;
    *p = *q;
    *q = *p;
  }
}

/* Red statuses depending on call contexts. */
void callstack (void) {
  int a = 1;
  int b = 42;
  int c = 42;
  int uninit;
  maybe_swap(&a, &b);
  maybe_swap(&a, &b);
  maybe_swap(&b, &c); // In [swap], the second assertion should be red.
  maybe_swap(&uninit, &a); // In [swap], alarm \initialized(*p) should be red.
  maybe_swap(&uninit, &b);
  maybe_swap(&a, 0); // In [swap], alarm \valid_read(q) should be red.
  int *p = nondet ? &a : (nondet ? &uninit : 0);
  int *q = nondet ? &b : (nondet ? &uninit : 0);
  maybe_swap(p, q); // This should not create other red alarms.
}

/* Red statuses depending on partitioning. */
void partitioning (void) {
  int t[32];
  int *p = &t[0];
  int i, r;
  //@ loop unroll 32;
  for (i = 0; i < 32; i++) {
    t[i] = i; // No alarm.
  }
  if (nondet)
    r = t[i]; // Invalid alarm: red status.
  p = &t[0];
  for (i = 0; i < 32; i++) {
    *p = i; // False alarm — so no red status.
    p++;
  }
  //@ loop unroll 0;
  for (i = 0; i <= 32 && nondet; i++) {
    t[i] = i; // True alarm, but no red status without partitioning.
  }
  //@ loop unroll 32;
  for (i = 0; i <= 32 && nondet; i++) {
    t[i] = i; // Alarm with a red status thanks to the loop unrolling.
  }
  //@ dynamic_split i == 32;
  for (i = 0; i <= 32 && nondet; i++) {
    t[i] = i; // Alarm with a red status thanks to the dynamic split.
  }
  if (i != 5)
    r = 100 / (i - 5); // False alarm — so no red status.
  r = 100 / (i - 10); // True alarm with no red status.
  //@ split i;
  r = 100 / (i - 15); // Alarm with red status thanks to the split.
}

/*@ requires positive: x >= 0;
    requires bounded: x < 100;
    assigns \nothing; */
void in_bound (int x) {
  int y = x;
  return;
}

void preconditions (void) {
  in_bound(42);
  in_bound(nondet);
  if (nondet)
    in_bound(-1); // First precondition has red status.
}

void user_assertions (void) {
  int x;
  //@ check indeterminate: x > 0;
  x = 1;
  //@ check true: x > 0;
  //@ check false: x < 0;
  x = nondet;
  //@ check maybe: x > 0;
}

void main (void) {
  callstack();
  partitioning();
  preconditions();
  user_assertions();
}

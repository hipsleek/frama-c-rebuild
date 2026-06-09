/*  run.config*
    COMMENT: Regression test for a fixed bug in the monitor analysis
*/

int a, c;
char b;

int fn1(long p);

int fn2(long p1) {
  a = fn1(p1);
  return a;
}

int main(void) {
  // [c] is monitored due to the assertion below, hence [fn2]'s return value
  // is monitored. Thus we must mark [fn2] itself as a monitored function to
  // ensure that calls to it are instrumented correctly. We already marked
  // functions as monitored if they contained monitored assignments or calls
  // to monitored functions, but we had forgotten to mark them for monitored
  // return statements.
  // This caused a mismatch in the number of formals in the transformed
  // version of [fn2] and the number of argument expressions in the
  // transformed version of this call.
  c = fn2(b);
  /*@ assert security_status(c) == public; */
  return 0;
}

/*  run.config*
    COMMENT: Regression test for a fixed bug in the monitor analysis
*/

void func(int *p) {
  // This assertion causes [p] to be monitored, but we did not propagate
  // this fact properly to the call site in [main]. Hence we used to
  // correctly generate label parameters for [func] but did not add the
  // corresponding label arguments at the call site in [main].

  /*@ assert security_status(p) == public; */
}

int main(void) {
  int y;
  func(&y);
  return 0;
}

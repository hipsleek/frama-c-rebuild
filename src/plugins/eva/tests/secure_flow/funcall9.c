/*  run.config*
    COMMENT: Test iterated monitor analysis
*/

extern int d;
extern int *e;

unsigned L = 5;

void fn1(int *p) {
  unsigned local_L = 5;
  *p = L;        // was: "failure: No list of labels mapped for Variable L"
  *p = local_L;  // was: "failure: empty list of lhost labels"
}

void main(void) {
  // Due to the assertion below, [e] must be monitored. The monitoring
  // status must be propagated to [d]. Thus [p] in [fn1] must be monitored
  // as well, and hence [L] and [local_L]. However, previously, this
  // information was not known at the time that [fn1] was analyzed, so all
  // these variables were not monitored, only [e] and [d].
  // This has been fixed by iterating the monitor analysis over the entire
  // program until a fixed point is reached. The second time around, [fn1]
  // is analyzed in a state where [d] is monitored, so [p] and the other
  // variables in [fn1] become monitored as well.
  e = &d;
  /*@ assert security_status(e) == public; */
  fn1(&d);
}

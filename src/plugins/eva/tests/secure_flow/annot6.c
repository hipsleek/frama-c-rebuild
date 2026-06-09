/*  run.config*
    COMMENT: Test security annotations in contracts for external functions.
*/

// based on an example from Julien

extern int __fc_private G1;
extern int G2;
int *P;

/*@ assigns \result \from *x, x; */
extern int f(int *x);

int main(void) {
  P = &G1;
  G2 = f(P);
  /*@ assert security_status(G2) == private; */
  return 0;
}

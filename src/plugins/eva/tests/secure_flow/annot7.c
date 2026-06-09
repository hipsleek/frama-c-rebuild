/*  run.config*
    COMMENT: Test security annotations in contracts for external functions.
*/

// more difficult version of annot6.c

extern int __fc_private G1;
extern int G2;
int *P;

/*@ assigns \result \from *x, x; */
extern int f(int *x);

int main(void) {
  int zero = 0, one = 1;
  P = (G1 ? &zero : &one);
  /*@ assert security_status(zero) == public; */
  /*@ assert security_status(one) == public; */
  /*@ assert security_status(P) == private; */
  /* The analysis must now resolve "\from *x, x" to {zero, one, P}, of which
     the P is especially important (because it is private). It is easy to
     map *x to {zero, one} using Value. To map x to P, the analysis finds
     the index of x in f's parameter list, then finds the corresponding
     actual argument expression. */
  G2 = f(P);
  /*@ assert security_status(G2) == private; */
  return 0;
}

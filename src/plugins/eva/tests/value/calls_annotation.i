/* run.config*
  STDOPT:
*/

/* This file tests the evaluation of the logical status of "calls" annotations,
   and the reduction of called functions accordingly. */

volatile unsigned int nondet;

int incr(int x) {
  return x+1;
}

int decr(int x) {
  return x-1;
}

int square(int x) {
  return x*x;
}

/* Should be never called according to "calls" annotations. */
int error(int x) {
  return x / 0;
}

int unreachable(int x) {
  return 0;
}

typedef int (*fptr)(int);

fptr funs[4] = { &incr, &decr, &square, &error };

int apply(fptr fp, int a) {
  int x;
  //@ calls incr, decr, square; // unknown: error could also be called
  x = (*fp)(a);
  //@ calls incr, decr, square; // valid as consequence of the previous one
  x = (*fp)(x);
  return x;
}

void main(void) {
  int a = 1, i = 0;

  /* Valid "calls" annotations. */
  //@ calls decr; // valid singleton
  a = (*funs[1])(a);
  i = nondet % 3;
  //@ calls incr, decr, square; // valid
  a = (*funs[i])(a);
  //@ calls incr, decr, square, unreachable; // valid but contains unreachable
  a = (*funs[i])(a);

  /* Unknown "calls" annotations: Eva can reduce the called functions. */
  i = nondet % 4;
  //@ calls incr, decr; // unknown: square and error could also be called
  a = (*funs[i])(a);
  Frama_C_show_each_0_1(i); // i can be reduced to {0; 1}

  /* Invalid "calls" annotations. */
  if (nondet) {
    //@ calls incr; // invalid
    a = (*funs[1])(a);
    Frama_C_show_each_UNREACHABLE();
  }
  if (nondet) {
    i = nondet % 2;
    //@ calls square, unreachable; // invalid
    a = (*funs[i])(a);
    Frama_C_show_each_UNREACHABLE(i);
  }

  /* "calls" annotations evaluated in multiple states. */
  //@ loop unroll 4;
  for (int j = 0; j < 4; j++) {
    a = 0;
    //@ calls incr; // valid at each iteration
    a = (*funs[0])(a);
    //@ calls incr, decr; // valid at each iteration
    a = (*funs[j % 2])(a);
    i = nondet % (j+1);
    //@ calls incr, decr, square; // valid then unknown at the last iteration
    a = (*funs[i])(a);
    if (nondet) {
      //@ calls incr, decr, square; // invalid at the last iteration
      a = (*funs[j])(a);
      if (j == 3) Frama_C_show_each_UNREACHABLE(j); // j == 3 is impossible here.
    }
    if (nondet) {
      a = apply(funs[j], a); // invalid at the last iteration
      if (j == 3) Frama_C_show_each_UNREACHABLE(j); // j == 3 is impossible here.
    }
    if (nondet) {
      //@ calls unreachable; // invalid at each iteration
      a = (*funs[j])(a);
      Frama_C_show_each_UNREACHABLE(a);
    }
  }
}

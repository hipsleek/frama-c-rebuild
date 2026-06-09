/* run.config*
  STDOPT: +""
*/

volatile int nondet;

int stop () {
 L: goto L;
}

void skip_declaration(void) {
  int y, r;
  if (nondet) {
    goto l; // This goto skips the declaration of variable x below.
  }
  int x = 1;
  y = 2;
  l: // x and y are both uninitialized when coming from the goto
  //@ check unknown: \initialized(&x);
  //@ check unknown: x > 0;
  //@ check unknown: \initialized(&y);
  //@ check unknown: y > 0;
  r = x + 1; // An initialization alarm must be emitted.
  r = y + 1; // An initialization alarm must be emitted.
  return;
}

int main() {
  volatile int c=0;
  c = c?1:0;

  if (c) stop ();

  skip_declaration ();
}

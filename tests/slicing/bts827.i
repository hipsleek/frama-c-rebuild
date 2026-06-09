/* run.config
   STDOPT: +"-slice-return main -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -no-deps"
*/

/* The problem was a mix-up between f outputs and return value. */

int G;

int f (void) {
  G = 3;
  return 5;
}

int main (void) {
  G = 1;
  G += f ();
  return G;
}


/* run.config
  STDOPT: +"-eva-precision 0"
  STDOPT: +"-eva-precision 3"
  STDOPT: +"-eva-precision 3 -eva-auto-loop-unroll 20 -eva-slevel 0 -eva-domains octagon"
  STDOPT: +"-eva-precision 3 -eva-msg-key=-precision-settings"
*/
/* run.config*
   DONTRUN: avoids many diff in the -eva-precision configuration message.
*/


/* Tests the meta-option -eva-precision.
   The third run tests that -eva-precision does not overwrite a specified
   parameter — even if the specified parameter is set to its default value. */

volatile unsigned int nondet;

void main (void) {
  int t[64];

  /* Requires -eva-auto-loop-unroll 64 to be precisely analyzed,
     automatically enabled with -eva-precision 3. */
  for (int i = 0; i < 64; i++) {
    t[i] = nondet % 100;
  }

  int index = nondet % 64;
  int r = t[index]; // No initialization alarm with -eva-precision 3.

  /* Pattern precisely interpreted with the symbolic locations domain,
     automatically enabled by -eva-precision >= 1. */
  if (t[index] == 42) {
    Frama_C_show_each_42(t[index]); // Singleton value with -eva-precision.
  }

  int k = r + index;
  int diff = k - r; // In [0..63] with the octagon domain.
}

/* Tests that the automatic configuration by -eva-verbose N does not overwrite
   message and warning categories set by the user. */

void main (void) {
  int x;
  // Partial unrolling.
  //@ loop unroll 3;
  for (int i = 0; i < 10; i++)
    x = i;
  // Automatic unrolling.
  for (int i = 0; i < 10; i++)
    x = i;
  Frama_C_show_each(x);
}

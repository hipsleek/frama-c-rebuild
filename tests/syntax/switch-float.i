/* run.config
 EXIT: 1
   STDOPT:
*/

void switch_float() {
  double x = -1.5;
  switch (x); // invalid, but must not crash
}

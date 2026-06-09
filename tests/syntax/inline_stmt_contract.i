/* run.config
OPT: -inline-stmt-contracts -print
*/

int X;

int f() {
  /*@ requires X == 0;
      ensures X == 1;
  */
  X++;
  return X;
}

int g() {

  /*@ behavior b:
        assumes X >= 0;
        requires X > -1;
        ensures X == 0;
   */
  if (X >= 0) { X = 0; } else { X = -1; }
  return X;
}

int h() {
  /*@ ensures \false; */
  return 1;
}

int i() {
  /*@ behavior b0:
        assumes X == 0;
        ensures X == 1;
  */
  X++;

  /*@ behavior b1:
        assumes X == 1;
        ensures X == 2;
  */
  X++;
  return X;
}

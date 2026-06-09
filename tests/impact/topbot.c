/* run.config
   STDOPT: +"-impact-annot main -pdg"
   */

//@ requires \false;
void f() { // Bottom PDG
}

void main(int c) {
  /*@ impact_stmt; */
  int x = 1;
  int y, z;
  if (c) {
    y = x;
    f();
    z = x;
  }
  z = x;
}

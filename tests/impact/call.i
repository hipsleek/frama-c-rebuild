/* run.config

   STDOPT: +"-impact-annot main"
   STDOPT: +"-impact-annot main2" +"-main main2"
   STDOPT: +"-impact-annot main3" +"-main main3"
   */

/*@ ghost int G; */

/*@ assigns G \from p; */
void p1 (int p);
void p2 (int);
int X;

void test (void) {
  if (X) p1(1); else p2(0);
}

/* ************************************************************************* */

void main (int x) {
  /*@ impact_stmt; */
  X = x;
  test ();
}

/* ************************************************************************* */

void call_test (void) {
  test ();
}

void main2(int x) {
  /*@ impact_stmt; */
  X = x;
  call_test ();
}

/* ************************************************************************* */

/*@ assigns G \from \nothing; */
void p3 (int);

void test3 (void) {
  if (X) p3(1); else p2(0);
}

void call_test3 (void) {
  test3 ();
}

void main3(int x) {
  /*@ impact_stmt; */
  X = x;
  call_test3 ();
}

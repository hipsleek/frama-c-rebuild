/* run.config
   STDOPT: +"-impact-annot g" +"-lib-entry" +"-main g"
   STDOPT: +"-impact-annot h" +"-lib-entry" +"-main h"
   */

int X;

int f(int x, int y) { X = x; return y; }

void g() {
  int a, b, c, d;
  b = 0;
  /*@ impact_stmt; */
  a = 0;
  c = f(a,b);
  d = X;
  c = f(a,d);
}

void h() {
  int a, b, c, d;
  /*@ impact_stmt; */
  b = 0;
  a = 0;
  c = f(a,b);
  d = X;
  c = f(a,d);
}

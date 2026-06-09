/* run.config*
   STDOPT: +"-eva-ilevel 8"
   STDOPT: +"-eva-ilevel 8 -cpp-extra-args='-DINTEGER'"
*/

int t[10];

extern int x;
volatile unsigned int nondet;

#ifdef INTEGER
#define INT integer
#else
#define INT int
#endif

void main() {
  /* Trivial assertions on 0 array. */

  //@ check true: \forall INT i; 0 <= i <= 9 ==> t[i] == 0;
  //@ check true: \exists INT i; 0 <= i <= 9 && t[i] == 0;
  //@ check false: \forall INT i; 0 <= i <= 9 ==> t[i] == 1;
  //@ check false: \exists INT i; 0 <= i <= 9 && t[i] == 1;
  //@ check unknown: \forall INT i; 0 <= i <= 9 ==> t[i] == x;
  //@ check unknown: \exists INT i; 0 <= i <= 9 && t[i] == x;

  /* Assertions on a precise array with enumeration of indexes. */

  //@ loop unroll 10;
  for (int k = 0; k < 10; k++) {
    t[k] = k;
  }
  //@ check true: \forall INT i; 0 < i < 8 ==> t[i] > 0;
  //@ check false: \forall INT i; 0 <= i < 8 ==> t[i] > 0;
  //@ check true: \exists INT i; 0 <= i < 8 && t[i] == 2;
  //@ check true: \exists INT i; 0 <= i < 8 && t[i] > 2;
  //@ check false: \exists INT i; 0 <= i < 8 && t[i] > 10;

  //@ check unknown: \forall INT i; 0 < i < 8 ==> t[i] > x;
  //@ check unknown: \exists INT i; 0 < i < 8 && t[i] > x;

  /* Same assertions but with too many indexes to enumerate. */

  //@ check true: \forall INT i; 0 < i < 10 ==> t[i] > 0;
  //@ check false: \forall INT i; 0 <= i < 10 ==> t[i] > 0;
  //@ check true: \exists INT i; 0 <= i < 10 && t[i] == 2;
  //@ check true: \exists INT i; 0 <= i < 10 && t[i] > 2;
  //@ check false: \exists INT i; 0 <= i < 10 && t[i] > 10;

  //@ check unknown: \forall INT i; 0 < i < 10 ==> t[i] > x;
  //@ check unknown: \exists INT i; 0 < i < 10 && t[i] == x;

  /* Assertions on an imprecise array. */

  //@ loop unroll 10;
  for (int k = 0; k < 10; k++) {
    t[k] = nondet % (k+1);
  }
  //@ check true: \forall INT i; 0 <= i < 8 ==> t[i] >= 0;
  //@ check unknown: \forall INT i; 1 < i < 8 ==> t[i] > 0;
  //@ check false: \forall INT i; 0 <= i < 8 ==> t[i] > 0;
  //@ check true: \exists INT i; 1 < i < 8 && t[i] < 6;
  //@ check unknown: \exists INT i; 0 <= i < 8 && t[i] == 2;
  //@ check false: \exists INT i; 0 <= i < 8 && t[i] > 10;

}

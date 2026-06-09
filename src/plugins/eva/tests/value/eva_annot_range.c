/* run.config*
   OPT: -eva -eva-precision 2 -eva-annot main -print
*/


/* -------------------------------------------------------------------------- */
/* --- Testing EVA Annotations                                            --- */
/* -------------------------------------------------------------------------- */

//@ ghost int world;
int a[20];

/*@
  ensures 0 <= \result <= 100;
  assigns \result,world \from world;
*/
int value(void);

int main(void) {
  int s = 0;
  for (int i = 0; i < 20; i+=2) {
    int v = value();
    a[i] = v;
    s += v;
  }
  return s;
}

/* -------------------------------------------------------------------------- */

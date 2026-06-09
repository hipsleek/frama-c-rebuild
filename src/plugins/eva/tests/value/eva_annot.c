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
  assigns \result,world \from world, root;
*/
int value(int root);

float main(void)
{
  float s = 0;
  for (int i = 0; i < 20; i++)
  {
    int v = value(i + 1);
    a[i] = i+v;
    s += v;
  }
  return s;
}

/* -------------------------------------------------------------------------- */

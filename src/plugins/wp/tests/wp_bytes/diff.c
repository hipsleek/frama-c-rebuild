/* run.config
   OPT: -wp-model typed -wp-fct f
   OPT: -wp-model bytes
*/

/* run.config_qualif
   OPT: -wp-model typed -wp-fct f
   OPT: -wp-model bytes
*/

/*@
  requires -10 <= i <= 10 && -10 <= j <= 10 ;
  ensures \result == i - j;
  assigns \nothing;
  */
int f (int *a, int i, int j)
{
  int *p = a + i ;
  int *q = a + j ;
  /*@ probe DIFF: p - q; */
  return p - q ;
}

/*@
  requires -10 <= i <= 10 && -10 <= j <= 10 ;
  ensures \result == (i - j) * sizeof(int);
  assigns \nothing;
  */
int g (int *a, int i, int j)
{
  char *p = a + i ;
  char *q = a + j ;
  /*@ probe DIFF: p - q; */
  return p - q ;
}

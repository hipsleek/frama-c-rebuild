/* run.config_qualif
   OPT: -wp -wp-par 1
*/

void f (void)
{
  int * p ;
  {
    int x ;
    p = &x ;
  }
  //@ assert A:!\valid(p);
}

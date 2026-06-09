/* run.config
   OPT:
   OPT: -wp-no-split-switch
*/

/* run.config_qualif
   DONTRUN:
*/

//@ predicate P(integer v);

/*@ ensures P(\result); */
int f(int x) {
  switch(x) {
  case 0:
  case 1:
  case 2:
    return 10;
  case 3:
    return 20;
  default:
    return 30;
  }
}

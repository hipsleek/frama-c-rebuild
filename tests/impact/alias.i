/* run.config
   STDOPT: +"-impact-annot f" +"-lib-entry" +"-main f" +"-eva-remove-redundant-alarms"
   */

int P,c;

/*@ requires \valid(x); */
int f(int *x) {
  /*@ impact_stmt; */
  int *y = x+1;
  *y = 4;
  int a = *(x+1) + 2;
  *y = 2;
  if (c)
    return *(x+1);
  else {
    y = P;
    return *y;
  }
}

/*@
  requires 0 <= n;
 */
void f (int n) {
  /*@
    loop invariant 0 <= i <= n;
    loop assigns i;
    loop variant n - i;
  */
  for (int i = 0; i < n; i++) {
    /*@ assert \at(i,LoopEntry) == 0; */
    int j = 0;
    /*@
      loop invariant 0 <= j <= i;
      loop assigns j;
      loop variant i - j;
    */
    while (j++ < i) {
      /*@ assert \at(j,LoopEntry) == 0; */
      /*@ assert \at(j,LoopCurrent) + 1 == j; */
    }
  }
}

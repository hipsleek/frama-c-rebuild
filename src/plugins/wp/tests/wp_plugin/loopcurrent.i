
/*@ requires (n+p) > 0; */
void f(int n,int p)
{
  int r;
  n += p;
  r = 0;
  /*@ 
    loop invariant \at(n,LoopEntry) == \at(n+p,Pre);
    loop invariant 0 ≤ n ≤ \at(n + p,Pre);
    loop invariant \at(n + r,LoopCurrent) == \at(n,LoopEntry);
    loop variant n;
  */
  while (n--) r++;
}

struct S { int p,q; };

/*@
  ensures P: \result.p == - (x->p);
  ensures Q: \result.q == x->q;
*/
struct S negate(struct S *x)
{
  struct S r = *x;
  r.p = - r.p ;
  return r;
}

/*@ predicate pointed(int *p, int* q) = p==q || \separated(p,q);
*/

/*@
  requires \valid(p);
  requires \valid(q);
  requires pointed(p,q);
  assigns  *p, *q;
  region   PQ: *p, *q;
  behavior EQ:
    assumes  p == q;
    ensures  P: *p == \old(*p);
    ensures  Q: *q == \old(*q);
  behavior SEP:
    assumes  p != q;
    ensures  Q: *p == \old(*p)+1;
    ensures  Q: *q == \old(*q)-1;
*/
void f(int* p, int* q) {
  (*p)++;
  (*q)--;
}

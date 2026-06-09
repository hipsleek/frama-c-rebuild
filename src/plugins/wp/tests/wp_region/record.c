struct S {
    int f;
    int g;
};

/*@ predicate pointed(struct S *p, struct S * q) = p==q || \separated(p,q);
*/


/*@ requires pointed(p,q);
    region PQf: p->f, q->f;
    ensures p != q ==> p->f == \old(p->f) + 1;
    ensures p == q ==> p->f == \old(p->f);
    ensures q->g == 0;
@*/
void f (struct S *p, struct S *q) {
  (p->f)++;
  (q->f)--;
  short *r = &(q->g);
  r[0] = -1;
  r[1] = -1;
  //@ assert q->g == -1;
  (q->g)++;
}

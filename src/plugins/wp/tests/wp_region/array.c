struct S {
    int len;
    int content[10];
};
/*@ predicate pointed(struct S *p, struct S * q) = p==q || \separated(p,q);
*/


/*@
    requires pointed(a, b);
    region AB: *a, *b;
    ensures a!=b ==> a->content[2] == \old(a->content[2]) + b->content[2];
    ensures a==b ==> a->content[2] == 2*\old(a->content[2]);
    ensures a->content[2] == \old(a->content[2] + b->content[2]);
    ensures a->content[4] == \old(a->content[4]);
*/
void add_first4(struct S * a , struct S * b )
{
    a->content[0] += b->content[0];
    a->content[1] += b->content[1];
    a->content[2] += b->content[2];
    a->content[3] += b->content[3];
}

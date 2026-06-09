// conditional with multiple levels of indirection
//  {a, b, c} are aliased
//  {*a, *b, *c, i, j} are aliased
//  {*(*a), *(*b), *(*c), *i, *j, x, y} are aliased
//  {*(*(*a)), *(*(*b)), *(*(*c)), *(*i), *(*j), *x, *y, t, u} are aliased



int main () {
  int ****a, ****b, ****c, ***i, ***j, **x, **y, *t, *u, p;
  if (p) {
    a=b;
    b=&i;
    i=&x;
    x=&t;
  }
  else {
    a=c;
    c=&j;
    j=&y;
    y=&u;
  }
  p=0;

  return 0;
}

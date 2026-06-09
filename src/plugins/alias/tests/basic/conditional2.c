// conditional cfg
//  {*a, *b, c} are aliased
//  {*(*a), *(*b), *c, d} are aliased
//  {a, b} are aliased


int main () {

  int ***a, ***b, **c, *d, e;
  b = &c;
  c = &d;
  d = &e;
  if (a) {
    a = b;
  }
  else {
    a = &c;
  }
  return 0;
}

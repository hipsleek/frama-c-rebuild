// double pointer assignment
//  {a, c} are aliased
//  {*a, *c, b, d} are aliased


int main () {

  int **a=0, *b=0, **c=0, *d=0;
  *a = b;
  *c = d;
  b = d;
  return 0;
}

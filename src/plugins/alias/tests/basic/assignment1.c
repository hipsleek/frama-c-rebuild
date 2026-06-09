// single pointer assignment
// {a, b, c, d} are aliased

int main () {

  int *a=0, *b=0, *c=0, *d=0;
  a = b;
  b = c;
  a = d;
  return 0;
}

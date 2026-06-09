// function with no return
//  {a, b} are aliased
//  {c, d} are aliased


void swap(int *x, int* y) {
  int*z=0;
  z=x;
  x=y;
  y=z;
}


int main(void)
{
  int *a=0, *b=0, *c=0, *d=0;
  swap(a,b);
  swap(c,d);
  return 0;
}

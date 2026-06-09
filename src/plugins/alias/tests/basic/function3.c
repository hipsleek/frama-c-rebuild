// function with a loop inside
//  {a, b} are aliased
//  {c, d} are aliased

void *f1(int *x, int* y)
{
  int *tmp = x;

  while (1) {
    x=y;
    y=tmp;
    break;
  }
  return (void *) 0;
}

int main(void)
{
  int *a=0, *b=0, *c=0, *d=0;
  f1(a,b);
  f1(c,d);
  return 0;
}

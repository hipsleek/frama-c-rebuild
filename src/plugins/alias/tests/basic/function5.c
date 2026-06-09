// function with multiple returns
//  {a, b, c} are aliased


int * choice(int* x, int* y)
{
  int c=0;
  if (c)
    return x;
  else
    return y;
}

int main(void)
{
  int *a=0, *b=0, *c=0;
  c = choice(a,b);
  return 0;
}

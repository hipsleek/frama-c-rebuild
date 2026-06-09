// function with address argument
//  {a, b} are aliased

int * addr(int* x)
{
    return x;
}

int main(void)
{
  int *a=0, *b=0, c=0;
  a = addr(&c);
  b = &c;
  return 0;
}

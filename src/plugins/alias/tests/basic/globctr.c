void f(int *x) {
  int y = 0;
  *x = y;
}

int main(void)
{
  int *a=0, *b=0;
  f(a);
  f(b);
  return 0;
}

void f(char **x, char **y, char **z)
{
  char *t;
  *y = t + (*y - *x);
  *z = t + (*z - *x);
  *x = t;
}

void g(char **a)
{
  f(a,a,a);
}

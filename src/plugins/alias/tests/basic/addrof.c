int main () {
  int **a, ***b, **y, *x, *z, p;
  a = &x;
  b = &y;
  if (p)
    y = &z;
  else
    *y = x;

  return 0;
}

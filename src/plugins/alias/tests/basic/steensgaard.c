// from steensgaard's paper
//  {*b, *c, a, y} are aliased
//  {b, c} are aliased


int main () {
  int *a, **b, **c, *y, x, z, p;
  a = &x;
  b = &y;
  if (p)
    y = &z;
  else
    y = &x;
  c = &y;
  *y = 4;
  return 0;
}



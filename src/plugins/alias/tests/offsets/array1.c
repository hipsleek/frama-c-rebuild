// simple array
//  {x, y} are aliased

int main () {
  int tab[4];
  tab[0] = 0;
  tab[1] = 1;
  tab[2] = tab[1] +1;

  int* x = &tab[1];
  int* y = &tab[2];
  tab[3] = *x + *y;

  return 0;
}

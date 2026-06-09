#include <stdio.h>

int *a;
int *b;
int *n;
int *(*z)[2];
int *(*q)[2];
int *t[2];

int main(void)
{
  t[0] = a;
  z = &t;
  q = z;
  b = *z[0];
  n = *q[0];
  printf("%d\n%d\n", a == b, a == n);
  return 0;
}

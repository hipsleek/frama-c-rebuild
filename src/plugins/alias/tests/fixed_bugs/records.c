#include <stdio.h>

typedef struct { int *field; } ty;

int x;
int *a;
int *b;
int *n;
ty *z;
ty *q;
ty t;

int main(void)
{
  a = &x;
  t.field = a;
  z = &t;
  q = z;
  b = z->field;
  n = q->field;
  printf("%d\n%d\n", a == b, a == n);
  return 0;
}
